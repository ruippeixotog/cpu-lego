package simulator

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

import core.*
import util.UnionFind

import GateProcessor.Event.*

object GateProcessor {

  enum Event {
    case PortChange(port: Port, value: Option[Boolean])
    case PortGroupDrive(group: PortGroup)
    case PortGroupCheck(group: PortGroup)
  }

  inline def setup(root: Component, extraWires: List[(Port, Port)] = Nil): GateProcessor =
    SimSetup.setup(Circuit(root, extraWires))

  inline def setupAndRun(root: Component, maxTicks: Option[Long] = None): GateProcessor =
    setup(root).run(maxTicks)
}

/** The functional gate-level simulation engine.
  *
  * A GateProcessor is an immutable, deterministic discrete-event simulation of a [[Circuit]]: every method returns a
  * new processor, time only moves forward through [[step]], [[runTo]] and [[run]], and nothing is shared between
  * threads. It is the reusable engine underneath live simulators (see [[Sim]]) and the workhorse of the test suite.
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
    private val groupValues: Map[PortGroup, Option[Boolean]] = Map().withDefaultValue(None)
) {

  private def schedule(after: Long, ev: GateProcessor.Event): GateProcessor =
    copy(events = events + ((t + after, events.getOrElse(t + after, Vector()) :+ ev)))

  inline def tick = t

  def get(port: Port): Option[Boolean] =
    portValues(port).orElse(groupValues(c.groupOf(port)))

  inline def get(bus: Bus): Vector[Option[Boolean]] = bus.map(get)
  inline def isLow(port: Port): Boolean = get(port) == Some(false)
  inline def isHigh(port: Port): Boolean = get(port) == Some(true)

  def set(port: Port, newValue: Option[Boolean]): GateProcessor =
    schedule(0, PortChange(port, newValue))

  inline def set(port: Port, newValue: Boolean): GateProcessor = set(port, Some(newValue))
  inline def unset(port: Port): GateProcessor = set(port, None)
  inline def toggle(port: Port): GateProcessor = set(port, get(port).map(!_))

  def set(bus: Bus, newValue: Seq[Boolean]): GateProcessor =
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
      case PortChange(_, _) => 2
      case PortGroupCheck(_) => 3
    }
    Some(copy(t = t1, events = events.tail).processEvents(sortedEvs))
  }

  private def processEvents(evs: Seq[GateProcessor.Event]): GateProcessor =
    evs.foldLeft(this)(_.processEvent(_))

  private def processEvent(ev: GateProcessor.Event): GateProcessor = ev match {
    case PortChange(port, newValue) =>
      if (newValue == portValues(port)) this
      else {
        copy(portValues = portValues + ((port, newValue)))
          .runObservers(port)
          .schedule(conf.wireDelay, PortGroupDrive(c.groupOf(port)))
      }

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
      val newValue = groupDrivenValues(group) match {
        case Nil | List(_) => // everything's fine
        case vs =>
          vs.distinct match {
            case List(v) => println("WARNING")
            case _ => throw new Exception("PUM")
          }
      }
      this
  }

  private def groupDrivenValues(group: PortGroup): List[Boolean] =
    c.portsOf(group).toList.map(portValues).flatten

  private def runObservers(group: PortGroup): GateProcessor =
    c.portsOf(group).foldLeft(this)(_.runObservers(_))

  private def runObservers(port: Port): GateProcessor =
    portObservers(port).foldLeft(this) { (sim1, f) => f(sim1) }
}
