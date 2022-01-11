package simulator

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

import core._
import util.UnionFind

import Sim.Event._

object Sim {
  val WireDelay = 1

  enum Event {
    case PortChange(port: Port, value: Option[Boolean])
    case PortGroupDrive(group: PortGroup)
  }

  inline def setup(root: Component, extraWires: List[(Port, Port)] = Nil): Sim =
    SimSetup.setup(Circuit(root, extraWires))

  inline def setupAndRun(root: Component, maxTicks: Option[Int] = None): Sim =
    setup(root).run(maxTicks)
}

final case class Sim(
    c: Circuit,
    t: Long = 0,
    events: TreeMap[Long, Vector[Sim.Event]] = TreeMap(),
    portValues: Map[Port, Option[Boolean]] = Map().withDefaultValue(None),
    portObservers: Map[Port, List[Sim => Sim]] = Map().withDefaultValue(Nil),
    groupValues: Map[PortGroup, Option[Boolean]] = Map().withDefaultValue(None)
) {

  private def schedule(after: Long, ev: Sim.Event): Sim =
    copy(events = events + ((t + after, events.getOrElse(t + after, Vector()) :+ ev)))

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
    }

    val next = sortedEvs.foldLeft(copy(t = t1, events = events.tail)) { case (st, ev) =>
      // println(s"t=$t1: $ev")
      ev match {
        case PortChange(port, newValue) =>
          if (newValue == st.portValues(port)) st
          else {
            st.portObservers(port)
              .foldLeft(st.copy(portValues = st.portValues + ((port, newValue)))) { (st, f) => f(st) }
              .schedule(Sim.WireDelay, PortGroupDrive(st.c.groupOf(port)))
          }

        case PortGroupDrive(group) =>
          val groupPorts = st.c.portsOf(group).toList
          val newValue = groupPorts.map(st.portValues).flatten match {
            case Nil => None
            case v :: Nil => Some(v)
            case vs =>
              vs.distinct match {
                case v :: Nil =>
                  println("WARNING")
                  Some(v)
                case _ =>
                  throw new Exception("PUM")
              }
          }
          if (newValue == st.groupValues(group)) st
          else {
            groupPorts.foldLeft(st.copy(groupValues = st.groupValues + ((group, newValue)))) { case (st, p) =>
              st.portObservers(p).foldLeft(st) { case (st2, f) => f(st2) }
            }
          }
      }
    }
    Some(next)
  }
}
