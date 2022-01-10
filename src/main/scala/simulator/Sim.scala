package simulator

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

import core._
import util.UnionFind

object Sim {
  val WireDelay = 1

  inline def setup(root: Component, extraWires: List[(Port, Port)] = Nil): SimState =
    SimSetup.setup(Circuit(root, extraWires))

  inline def setupAndRun(root: Component, maxTicks: Option[Int] = None): SimState =
    run(setup(root), maxTicks)

  @tailrec def run(st: SimState, maxTicks: Option[Int] = None): SimState =
    step(st, maxTicks) match {
      case None => st
      case Some(next) => run(next, maxTicks)
    }

  def step(st: SimState, maxTicks: Option[Int] = None): Option[SimState] = {
    if (st.events.isEmpty) return None

    val (t1, evs) = st.events.head
    if (maxTicks.exists(t1 > _)) return None

    val sortedEvs = evs.sortBy {
      case PortGroupDrive(_) => 1
      case PortChange(_, _) => 2
    }

    val next = sortedEvs.foldLeft(st.copy(t = t1, events = st.events.tail)) { case (st, ev) =>
      // println(s"t=$t1: $ev")
      ev match {
        case PortChange(port, newValue) =>
          if (newValue == st.portValues(port)) st
          else st.set(port, newValue).schedule(WireDelay, PortGroupDrive(st.c.groupOf(port)))

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

trait SimEvent
case class PortChange(port: Port, value: Option[Boolean]) extends SimEvent
case class PortGroupDrive(group: PortGroup) extends SimEvent

case class SimState(
    c: Circuit,
    t: Long = 0,
    events: TreeMap[Long, Vector[SimEvent]] = TreeMap(),
    portValues: Map[Port, Option[Boolean]] = Map().withDefaultValue(None),
    portObservers: Map[Port, List[SimState => SimState]] = Map().withDefaultValue(Nil),
    groupValues: Map[PortGroup, Option[Boolean]] = Map().withDefaultValue(None)
) {

  def get(port: Port): Option[Boolean] = {
    portValues(port).orElse(groupValues(c.groupOf(port)))
  }

  def set(port: Port, value: Option[Boolean]): SimState = {
    portObservers(port).foldLeft(copy(portValues = portValues + ((port, value)))) { case (st, f) => f(st) }
  }

  def watch(port: Port, callback: SimState => SimState): SimState =
    copy(portObservers = portObservers + ((port, callback :: portObservers.getOrElse(port, Nil))))

  def schedule(after: Long, ev: SimEvent): SimState =
    copy(events = events + ((t + after, events.getOrElse(t + after, Vector()) :+ ev)))
}
