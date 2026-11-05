package simulator

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

import core.*

import GateProcessor.Event.*

object GateProcessor {

  enum Event {
    case PortChange(port: Port, value: Option[Boolean])
    case DeviceDrive(device: Device, port: Port, value: Option[Boolean])
    case PortGroupDrive(group: PortGroup)
    case PortGroupCheck(group: PortGroup)
  }

  /** Build a GateProcessor for the circuit: install gate behavior and tie the High/Low constant ports. High/Low wiring
    * is an implementation detail of this engine, not a general simulator concern.
    *
    * @param portNames
    *   hierarchical port names used in fault reports; ports missing from the map fall back to an identity-based
    *   placeholder
    */
  def setup(c: Circuit, conf: Config, portNames: Map[Port, String]): GateProcessor = {
    val p = GateProcessor(c, conf, portNames = portNames).set(High, true).set(Low, false)
    import p.conf._

    c.components.foldLeft(p) {

      case (p, NAND(in1, in2, out)) =>
        binaryOp(p, in1, in2, out, gateDelay) {
          case (Some(false), _) => Some(true)
          case (_, Some(false)) => Some(true)
          case (Some(true), Some(true)) => Some(false)
          case _ => None
        }

      case (p, Clock(halfPeriod, out, initialLevel)) =>
        p.set(out, initialLevel).watch(out)(_.toggleAfter(halfPeriod, out))

      case (p, Switch(in, out, enable)) =>
        binaryOp(p, enable, in, out, 0) {
          case (Some(true), v) => v
          case _ => None
        }
    }
  }

  private def binaryOp(p: GateProcessor, port1: Port, port2: Port, out: Port, delay: Int)(
      f: (Option[Boolean], Option[Boolean]) => Option[Boolean]
  ): GateProcessor = {

    def propagate(s: GateProcessor): GateProcessor =
      s.setAfter(delay, out, f(s.get(port1), s.get(port2)))

    p.watch(port1)(propagate).watch(port2)(propagate)
  }

  inline def setup(root: Component, extraWires: List[(Port, Port)] = Nil): GateProcessor = {
    val portNames = Index(root).ports.map { case (name, port) => port -> name }.toMap
    setup(Circuit(root, extraWires), Config.default, portNames)
  }

  inline def setupAndRun(root: Component, maxTicks: Option[Long] = None): GateProcessor =
    setup(root).run(maxTicks)
}

/** The functional gate-level simulation engine.
  *
  * A GateProcessor is an immutable, deterministic discrete-event simulation of a [[Circuit]]: every method returns a
  * new processor, time only moves forward through [[step]], [[runTo]] and [[run]], and nothing is shared between
  * threads. It is the reusable engine underneath live simulators (see [[Sim]]) and the workhorse of the test suite.
  *
  * Simulation time is measured in ticks; delays and timestamps throughout are in ticks.
  *
  * It is deliberately not a [[Sim]]: a Sim is a live, running simulation that peripherals interact with concurrently,
  * while a GateProcessor is a pure value that tests and simulators drive explicitly.
  */
final case class GateProcessor(
    c: Circuit,
    conf: Config = Config.default,
    private val t: Long = 0,
    private val events: TreeMap[Long, Vector[GateProcessor.Event]] = TreeMap(),
    private val portValues: Map[Port, Option[Boolean]] = Map().withDefaultValue(None),
    private val portObservers: Map[Port, List[GateProcessor => GateProcessor]] = Map().withDefaultValue(Nil),
    private val groupValues: Map[PortGroup, Option[Boolean]] = Map().withDefaultValue(None),
    private val portNames: Map[Port, String] = Map.empty,
    /** Faults recorded while processing events, in recording order: contention, non-convergence, device failures.
      * Recorded instead of printed or thrown, so callers observe them explicitly.
      */
    val faults: Vector[Fault] = Vector.empty,
    /** Attached devices, in attach order. A device's context and driven pins travel with its entry. */
    val devices: Vector[DeviceEntry] = Vector.empty
) extends EngineState {

  private def schedule(after: Long, ev: GateProcessor.Event): GateProcessor =
    copy(events = events + ((t + after, events.getOrElse(t + after, Vector()) :+ ev)))

  inline def tick = t

  def get(port: Port): Option[Boolean] =
    portValues(port).orElse(groupValues(c.groupOf(port)))

  inline def get(bus: Ports): Vector[Option[Boolean]] = bus.map(get)
  inline def isLow(port: Port): Boolean = get(port) == Some(false)
  inline def isHigh(port: Port): Boolean = get(port) == Some(true)

  def set(port: Port, newValue: Option[Boolean]): GateProcessor =
    schedule(0, PortChange(port, newValue))

  inline def set(port: Port, newValue: Boolean): GateProcessor = set(port, Some(newValue))
  inline def unset(port: Port): GateProcessor = set(port, None)
  inline def toggle(port: Port): GateProcessor = set(port, get(port).map(!_))

  def set(bus: Ports, newValue: Seq[Boolean]): GateProcessor =
    bus.zip(newValue).foldLeft(this) { case (sim1, (p, v)) => sim1.set(p, v) }

  def setAfter(after: Long, port: Port, newValue: Option[Boolean]): GateProcessor =
    schedule(after, PortChange(port, newValue))

  inline def setAfter(after: Long, port: Port, newValue: Boolean): GateProcessor = setAfter(after, port, Some(newValue))
  inline def unsetAfter(after: Long, port: Port): GateProcessor = setAfter(after, port, None)
  inline def toggleAfter(after: Long, port: Port): GateProcessor = setAfter(after, port, get(port).map(!_))

  def watch(port: Port)(callback: GateProcessor => GateProcessor): GateProcessor =
    copy(portObservers = portObservers + ((port, callback :: portObservers.getOrElse(port, Nil))))

  /** Process every event batch with timestamp <= `deadline`, in timestamp order. Batches with timestamp > `deadline`
    * are not processed.
    */
  def runTo(deadline: Long): GateProcessor = {
    var s: GateProcessor = this
    var next = s.step(Some(deadline))
    while (next.isDefined) {
      s = next.get
      next = s.step(Some(deadline))
    }
    s
  }

  @tailrec def run(maxTicks: Option[Long] = None): GateProcessor =
    step(maxTicks) match {
      case None => this
      case Some(next) => next.run(maxTicks)
    }

  /** Process the next scheduled event batch (all events at the smallest timestamp with pending events). If no events
    * are scheduled, return unchanged.
    */
  def step(): GateProcessor = step(None).getOrElse(this)

  def step(maxTicks: Option[Long] = None): Option[GateProcessor] = {
    if (events.isEmpty) return None

    val (t1, evs) = events.head
    if (maxTicks.exists(t1 > _)) return None

    val sortedEvs = evs.sortBy {
      case PortGroupDrive(_) => 1
      case PortChange(_, _) | DeviceDrive(_, _, _) => 2
      case PortGroupCheck(_) => 3
    }
    // Ports whose effective value this batch may change: a port's own drive, or every port of a driven group.
    val candidatePorts: Set[Port] = sortedEvs.flatMap {
      case PortChange(port, _) => Set(port)
      case DeviceDrive(_, port, _) => Set(port)
      case PortGroupDrive(group) => c.portsOf(group)
      case PortGroupCheck(_) => Set.empty
    }.toSet
    val post = copy(t = t1, events = events.tail).processEvents(sortedEvs)
    Some(post.runDeviceCallbacks(this, candidatePorts))
  }

  /** Run the device callbacks for this batch's net changes, in attach order then registration order. Each callback runs
    * after the whole batch, seeing the post-batch values, and drives issued through its context schedule into the
    * threaded processor.
    */
  private def runDeviceCallbacks(before: GateProcessor, candidates: Set[Port]): GateProcessor = {
    val fired = for {
      entry <- devices
      reg <- entry.ctx.registrations
      if reg.fires(candidates, before, this)
    } yield (entry, reg)
    var s: GateProcessor = this
    fired.foreach { case (entry, reg) =>
      entry.ctx.current = s
      reg.run()
      s = entry.ctx.current
    }
    s
  }

  /** Attach a device: settle the simulation to the current tick so `onAttach` sees the current levels, then run
    * `onAttach` at the current instant. Attaching an already-attached device is a no-op.
    */
  def attach(device: Device): GateProcessor = {
    if (devices.exists(_.device eq device)) this
    else {
      val settled = runTo(t)
      val ctx = new DeviceContextImpl(device)
      val withDevice = settled.copy(devices = settled.devices :+ DeviceEntry(device, ctx, Map.empty))
      ctx.current = withDevice
      device.onAttach(ctx)
      ctx.current
    }
  }

  /** Detach a device: cancel its pending drives, release every pin it currently drives, remove its sensitivities, then
    * run `onDetach`. Detaching a device that is not attached is a no-op. The release takes effect at once: the pins'
    * groups re-resolve after the wire delay, and no synthetic callbacks fire for the release itself.
    */
  def detach(device: Device): GateProcessor =
    devices.find(_.device eq device) match {
      case None => this
      case Some(entry) =>
        val noPending = copy(events = events.flatMap { case (tick, evs) =>
          val kept = evs.filterNot {
            case DeviceDrive(d, _, _) => d eq device
            case _ => false
          }
          if (kept.nonEmpty) Some(tick -> kept) else None
        })
        val released = entry.driven.foldLeft(noPending) { case (s, (port, value)) =>
          if (s.portValues(port) == Some(value))
            s.copy(portValues = s.portValues + ((port, None)))
              .runObservers(port)
              .schedule(conf.wireDelay, PortGroupDrive(c.groupOf(port)))
          else s
        }
        val removed = released.copy(devices = released.devices.filterNot(_.device eq device))
        device.onDetach()
        removed
    }

  /** Schedule a device drive, cancelling the device's pending drives of the same pin scheduled at or after the new
    * drive's time (inertial drives). Called only through a [[DeviceContext]], which enforces `delay >= 1`.
    */
  private[simulator] def driveDevice(device: Device, port: Port, value: Option[Boolean], delay: Int): GateProcessor = {
    val target = t + delay
    val pruned = events.flatMap { case (tick, evs) =>
      if (tick < target) Some(tick -> evs)
      else {
        val kept = evs.filterNot {
          case DeviceDrive(d, p, _) => (d eq device) && (p eq port)
          case _ => false
        }
        if (kept.nonEmpty) Some(tick -> kept) else None
      }
    }
    copy(events = pruned).schedule(delay, DeviceDrive(device, port, value))
  }

  private def processEvents(evs: Seq[GateProcessor.Event]): GateProcessor =
    evs.foldLeft(this)(_.processEvent(_))

  private def processEvent(ev: GateProcessor.Event): GateProcessor = ev match {
    case PortChange(port, newValue) =>
      applyPortChange(port, newValue)

    case DeviceDrive(device, port, newValue) =>
      applyPortChange(port, newValue).updateDriven(device, port, newValue)

    case PortGroupDrive(group) =>
      val (sim1, newValue) = groupDrivenValues(group) match {
        case Nil => (this, None)
        case v :: Nil => (this, Some(v))
        case vs =>
          (
            schedule(conf.scTolerance, PortGroupCheck(group)),
            vs.distinct match {
              case v :: Nil => Some(v)
              case _ => None
            }
          )
      }
      if (newValue == sim1.groupValues(group)) sim1
      else {
        sim1
          .copy(groupValues = sim1.groupValues + ((group, newValue)))
          .runObservers(group)
      }

    case PortGroupCheck(group) =>
      // The check runs scTolerance after the drive: a contention that settled on its own is not a fault. A
      // persistent disagreement is recorded as a Contention fault instead of being thrown; agreeing multiple drivers
      // are not a fault at all.
      groupDrivenValues(group).distinct match {
        case Nil | List(_) => this
        case values =>
          val nets = c.portsOf(group).toList.map(nameOf).sorted
          copy(faults = faults :+ Fault.Contention(nets, values.sorted))
      }
  }

  /** Apply a port drive: update the driven value, propagate through watching gates, and schedule the group
    * re-resolution after the wire delay. A drive to the current value is a no-op.
    */
  private def applyPortChange(port: Port, newValue: Option[Boolean]): GateProcessor =
    if (newValue == portValues(port)) this
    else {
      copy(portValues = portValues + ((port, newValue)))
        .runObservers(port)
        .schedule(conf.wireDelay, PortGroupDrive(c.groupOf(port)))
    }

  /** Record a device drive that took effect, so detach knows which pins the device currently drives. A release removes
    * the pin; a drive by a detached device (its pending drives were cancelled, so this should not happen) leaves the
    * bookkeeping alone.
    */
  private def updateDriven(device: Device, port: Port, newValue: Option[Boolean]): GateProcessor = {
    val idx = devices.indexWhere(_.device eq device)
    if (idx < 0) this
    else {
      val entry = devices(idx)
      val driven =
        newValue match {
          case Some(v) => entry.driven + ((port, v))
          case None => entry.driven - port
        }
      copy(devices = devices.updated(idx, entry.copy(driven = driven)))
    }
  }

  /** The hierarchical name of the port for fault reports, from the netlist name table; ports with no registered name
    * fall back to an identity-based placeholder.
    */
  private def nameOf(port: Port): String =
    portNames.getOrElse(port, s"<${Integer.toHexString(System.identityHashCode(port))}>")

  private def groupDrivenValues(group: PortGroup): List[Boolean] =
    c.portsOf(group).toList.map(portValues).flatten

  private def runObservers(group: PortGroup): GateProcessor =
    c.portsOf(group).foldLeft(this)(_.runObservers(_))

  private def runObservers(port: Port): GateProcessor =
    portObservers(port).foldLeft(this) { (sim1, f) => f(sim1) }
}
