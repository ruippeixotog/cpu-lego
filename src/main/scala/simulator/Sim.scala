package simulator

import scala.collection.immutable.TreeMap
import scala.collection.mutable.PriorityQueue

import core._
import util.UnionFind

case class PortGroup(root: Port)

case class Circuit(components: List[BaseComponent], wires: List[(Port, Port)]) {
  private lazy val uf = wires.foldLeft(UnionFind[Port]())(_.merge.tupled(_))

  lazy val groupOf: Port => PortGroup =
    uf.parents.keySet.map { p => (p, PortGroup(uf.root(p))) }.toMap.withDefault(PortGroup.apply)

  lazy val portsOf: PortGroup => Set[Port] =
    (uf.parents.keySet ++ uf.parents.values).groupBy(groupOf).withDefault { pg => Set(pg.root) }
}

object Sim {
  val GateDelay = 1
  val WireDelay = 1
  val PosEdgeDelay = 1

  def runComponent(root: Component, maxTicks: Option[Int] = None): SimState = {
    runCircuit(build(root), maxTicks)
  }

  def runCircuit(c: Circuit, maxTicks: Option[Int] = None): SimState = {
    run(setup(c), maxTicks)
  }

  def build(root: Component, extraWires: List[(Port, Port)] = Nil): Circuit = {
    root match {
      case comp: BaseComponent => Circuit(List(comp), extraWires)
      case comp: CompositeComponent =>
        comp.components.map(build(_)).fold(Circuit(Nil, extraWires ++ comp.wires)) { (c1, c2) =>
          Circuit(c1.components ++ c2.components, c1.wires ++ c2.wires)
        }
    }
  }

  def setup(c: Circuit): SimState = {
    val state = new SimState(c)

    state.schedule(0, PortChange(High, Some(true)))
    state.schedule(0, PortChange(Low, Some(false)))

    c.components.foreach {
      case nand: NAND =>
        def propagate(v1o: Option[Boolean], v2o: Option[Boolean]): Unit = {
          val res = (v1o, v2o) match {
            case (Some(false), _) => Some(true)
            case (_, Some(false)) => Some(true)
            case (Some(true), Some(true)) => Some(false)
            case _ => None
          }
          state.schedule(GateDelay, PortChange(nand.out, res))
        }
        state.watch(nand.in1, v1o => propagate(v1o, state.get(nand.in2)))
        state.watch(nand.in2, v2o => propagate(state.get(nand.in1), v2o))

      case ff: FlipFlop =>
        def propagate(set: Option[Boolean], reset: Option[Boolean]): Unit = {
          val res = (set, reset) match {
            case (Some(true), Some(false)) => Some(true)
            case (Some(false), Some(true)) => Some(false)
            case _ => None
          }
          res.foreach { v =>
            state.schedule(GateDelay, PortChange(ff.q, Some(v)))
            state.schedule(GateDelay, PortChange(ff.nq, Some(!v)))
          }
        }
        state.watch(ff.set, v1o => propagate(v1o, state.get(ff.reset)))
        state.watch(ff.reset, v2o => propagate(state.get(ff.set), v2o))

      case clock: Clock =>
        state.schedule(0, PortChange(clock.out, Some(true)))
        state.watch(
          clock.out,
          v => state.schedule(clock.freq, PortChange(clock.out, v.map(!_)))
        )

      case posEdge: PosEdge =>
        state.schedule(0, PortChange(posEdge.out, Some(false)))
        state.watch(
          posEdge.in,
          {
            case Some(true) =>
              state.schedule(GateDelay, PortChange(posEdge.out, Some(true)))
              state.schedule(GateDelay + PosEdgeDelay, PortChange(posEdge.out, Some(false)))
            case _ =>
            // do nothing
          }
        )

      case switch: Switch =>
        state.watch(
          switch.enable,
          {
            case Some(true) => state.schedule(0, PortChange(switch.out, state.get(switch.in)))
            case _ => state.schedule(0, PortChange(switch.out, None))
          }
        )
        state.watch(
          switch.in,
          v => {
            if (state.get(switch.enable) == Some(true)) {
              state.schedule(0, PortChange(switch.out, v))
            }
          }
        )
    }
    state
  }

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
