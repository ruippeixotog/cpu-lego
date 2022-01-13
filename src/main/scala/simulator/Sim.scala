package simulator

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

import core._
import util.UnionFind

import Sim.Event._

object Sim {

  enum Event {
    case PortChange(port: Port, value: Option[Boolean])
    case PortGroupDrive(group: PortGroup)
    case PortGroupCheck(group: PortGroup)
  }

  inline def setup(root: Component, extraWires: List[(Port, Port)] = Nil): Sim =
    SimSetup.setup(Circuit(root, extraWires))

  inline def setupAndRun(root: Component, maxTicks: Option[Int] = None): Sim =
    setup(root).run(maxTicks)
}

final case class Sim(
    c: Circuit,
    conf: Config = Config.default,
    private val t: Long = 0,
    private val events: TreeMap[Long, Vector[Sim.Event]] = TreeMap(),
    private val portValues: Map[Port, Option[Boolean]] = Map().withDefaultValue(None),
    private val portObservers: Map[Port, List[Sim => Sim]] = Map().withDefaultValue(Nil),
    private val groupValues: Map[PortGroup, Option[Boolean]] = Map().withDefaultValue(None)
) {

  private def schedule(after: Long, ev: Sim.Event): Sim =
    copy(events = events + ((t + after, events.getOrElse(t + after, Vector()) :+ ev)))

  inline def tick = t

  def get(port: Port): Option[Boolean] =
    portValues(port).orElse(groupValues(c.groupOf(port)))

  inline def get(bus: Vector[Port]): Vector[Option[Boolean]] = bus.map(get)
  inline def isLow(port: Port): Boolean = get(port) == Some(false)
  inline def isHigh(port: Port): Boolean = get(port) == Some(true)

  def set(port: Port, newValue: Option[Boolean]): Sim =
    schedule(0, PortChange(port, newValue))

  inline def set(port: Port, newValue: Boolean): Sim = set(port, Some(newValue))
  inline def unset(port: Port): Sim = set(port, None)
  inline def toggle(port: Port): Sim = set(port, get(port).map(!_))

  def setAfter(after: Long, port: Port, newValue: Option[Boolean]): Sim =
    schedule(after, PortChange(port, newValue))

  inline def setAfter(after: Long, port: Port, newValue: Boolean): Sim = setAfter(after, port, Some(newValue))
  inline def unsetAfter(after: Long, port: Port): Sim = setAfter(after, port, None)
  inline def toggleAfter(after: Long, port: Port): Sim = setAfter(after, port, get(port).map(!_))

  def watch(port: Port)(callback: Sim => Sim): Sim =
    copy(portObservers = portObservers + ((port, callback :: portObservers.getOrElse(port, Nil))))

  @tailrec def run(maxTicks: Option[Int] = None): Sim =
    step(maxTicks) match {
      case None => this
      case Some(next) => next.run(maxTicks)
    }

  def step(maxTicks: Option[Int] = None): Option[Sim] = {
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

  private def processEvents(evs: Seq[Sim.Event]): Sim =
    evs.foldLeft(this)(_.processEvent(_))

  private def processEvent(ev: Sim.Event): Sim = ev match {
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

  private def runObservers(group: PortGroup): Sim =
    c.portsOf(group).foldLeft(this)(_.runObservers(_))

  private def runObservers(port: Port): Sim =
    portObservers(port).foldLeft(this) { (sim1, f) => f(sim1) }
}
