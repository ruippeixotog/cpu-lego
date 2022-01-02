package simulator

import scala.collection.immutable.TreeMap
import scala.collection.mutable.PriorityQueue

import core._
import util.UnionFind

object Sim {
  val WireDelay = 1

  inline def runComponent(root: Component, maxTicks: Option[Int] = None): SimState =
    runCircuit(build(root), maxTicks)

  inline def runCircuit(c: Circuit, maxTicks: Option[Int] = None): SimState =
    run(setup(c), maxTicks)

  inline def build(root: Component, extraWires: List[(Port, Port)] = Nil): Circuit =
    Circuit(root, extraWires)

  inline def setup(c: Circuit): SimState =
    SimSetup.setup(c)

  def run(st: SimState, maxTicks: Option[Int] = None): SimState = {
    if (st.debug) println("t=0: start simulation")

    while (!st.events.isEmpty) {
      val (t1, evs) = st.events.head
      if (maxTicks.exists(t1 > _)) return st

      st.events = st.events.tail
      st.t = t1

      val sortedEvs = evs.sortBy {
        case PortGroupDrive(_) => 1
        case PortChange(_, _) => 2
      }

      for (ev <- sortedEvs) {
        if (st.debug) println(s"t=$t1: $ev")

        ev match {
          case PortChange(port, newValue) =>
            if (newValue != st.portValues(port)) {
              st.set(port, newValue)
              st.schedule(WireDelay, PortGroupDrive(st.c.groupOf(port)))
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
            if (newValue != st.groupValues(group)) {
              st.groupValues += ((group, newValue))
              groupPorts.foreach { port =>
                st.portObservers(port).foreach(_(newValue))
              }
            }
        }
      }
    }
    st
  }
}

trait SimEvent
case class PortChange(port: Port, value: Option[Boolean]) extends SimEvent
case class PortGroupDrive(group: PortGroup) extends SimEvent

class SimState(val c: Circuit) {
  var t: Long = 0
  // val events: PriorityQueue[(Long, SimEvent)] = PriorityQueue.empty(Ordering.by(-_._1))
  var events: TreeMap[Long, Vector[SimEvent]] = TreeMap()
  var portValues: Map[Port, Option[Boolean]] = Map().withDefaultValue(None)
  var portObservers: Map[Port, List[Option[Boolean] => Unit]] = Map().withDefaultValue(Nil)
  var groupValues: Map[PortGroup, Option[Boolean]] = Map().withDefaultValue(None)
  var debug: Boolean = false

  def get(port: Port): Option[Boolean] = {
    portValues(port).orElse(groupValues(c.groupOf(port)))
  }

  def set(port: Port, value: Option[Boolean]) = {
    portValues += ((port, value))
    portObservers(port).foreach(_(value))
  }

  def watch(port: Port, callback: Option[Boolean] => Unit): Unit =
    portObservers += ((port, callback :: portObservers.getOrElse(port, Nil)))

  def schedule(after: Long, ev: SimEvent) =
    events += ((t + after, events.getOrElse(t + after, Vector()) :+ ev))
}
