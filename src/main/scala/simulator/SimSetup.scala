package simulator

import core._

object SimSetup {
  val GateDelay = 1
  val PosEdgeDelay = 1

  def setup(c: Circuit): SimState = {
    val state = new SimState(c)
      .schedule(0, PortChange(High, Some(true)))
      .schedule(0, PortChange(Low, Some(false)))

    c.components.foldLeft(state) {
      case (state, nand: NAND) => setupNand(state, nand)
      case (state, ff: FlipFlop) => setupFlipFlop(state, ff)
      case (state, clock: Clock) => setupClock(state, clock)
      case (state, posEdge: PosEdge) => setupPosEdge(state, posEdge)
      case (state, switch: Switch) => setupSwitch(state, switch)
    }
  }

  def setupNand(state: SimState, nand: NAND): SimState = {
    def propagate(st: SimState): SimState = {
      val res = (st.get(nand.in1), st.get(nand.in2)) match {
        case (Some(false), _) => Some(true)
        case (_, Some(false)) => Some(true)
        case (Some(true), Some(true)) => Some(false)
        case _ => None
      }
      st.schedule(GateDelay, PortChange(nand.out, res))
    }
    state.watch(nand.in1, propagate).watch(nand.in2, propagate)
  }

  def setupFlipFlop(state: SimState, ff: FlipFlop): SimState = {
    def propagate(st: SimState): SimState = {
      val res = (st.get(ff.set), st.get(ff.reset)) match {
        case (Some(true), Some(false)) => Some(true)
        case (Some(false), Some(true)) => Some(false)
        case _ => None
      }
      res.fold(st) { v =>
        st.schedule(GateDelay, PortChange(ff.q, Some(v)))
          .schedule(GateDelay, PortChange(ff.nq, Some(!v)))
      }
    }
    state.watch(ff.set, propagate).watch(ff.reset, propagate)
  }

  def setupClock(state: SimState, clock: Clock): SimState = {
    state
      .schedule(0, PortChange(clock.out, Some(true)))
      .watch(
        clock.out,
        st => st.schedule(clock.freq, PortChange(clock.out, st.get(clock.out).map(!_)))
      )
  }

  def setupPosEdge(state: SimState, posEdge: PosEdge): SimState = {
    state
      .schedule(0, PortChange(posEdge.out, Some(false)))
      .watch(
        posEdge.in,
        st => {
          st.get(posEdge.in) match {
            case Some(true) =>
              st.schedule(GateDelay, PortChange(posEdge.out, Some(true)))
                .schedule(GateDelay + PosEdgeDelay, PortChange(posEdge.out, Some(false)))
            case _ =>
              st
          }
        }
      )
  }

  def setupSwitch(state: SimState, switch: Switch): SimState = {
    def propagate(st: SimState): SimState = {
      val res = (st.get(switch.enable), st.get(switch.in)) match {
        case (Some(true), v) => v
        case _ => None
      }
      st.schedule(0, PortChange(switch.out, res))
    }
    state.watch(switch.enable, propagate).watch(switch.in, propagate)
  }
}
