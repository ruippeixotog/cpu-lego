package simulator

import core._

object SimSetup {
  val GateDelay = 1
  val PosEdgeDelay = 1

  def setup(c: Circuit): Sim = {
    val sim = Sim(c).set(High, true).set(Low, false)

    c.components.foldLeft(sim) {
      case (sim, nand: NAND) => setupNand(sim, nand)
      case (sim, ff: FlipFlop) => setupFlipFlop(sim, ff)
      case (sim, clock: Clock) => setupClock(sim, clock)
      case (sim, posEdge: PosEdge) => setupPosEdge(sim, posEdge)
      case (sim, switch: Switch) => setupSwitch(sim, switch)
    }
  }

  def setupNand(sim: Sim, nand: NAND): Sim = {
    def propagate(s: Sim): Sim = {
      val res = (s.get(nand.in1), s.get(nand.in2)) match {
        case (Some(false), _) => Some(true)
        case (_, Some(false)) => Some(true)
        case (Some(true), Some(true)) => Some(false)
        case _ => None
      }
      s.setAfter(GateDelay, nand.out, res)
    }
    sim.watch(nand.in1, propagate).watch(nand.in2, propagate)
  }

  def setupFlipFlop(sim: Sim, ff: FlipFlop): Sim = {
    def propagate(s: Sim): Sim = {
      val res = (s.get(ff.set), s.get(ff.reset)) match {
        case (Some(true), Some(false)) => Some(true)
        case (Some(false), Some(true)) => Some(false)
        case _ => None
      }
      res.fold(s) { v =>
        s.setAfter(GateDelay, ff.q, v).setAfter(GateDelay, ff.nq, !v)
      }
    }
    sim.watch(ff.set, propagate).watch(ff.reset, propagate)
  }

  def setupClock(sim: Sim, clock: Clock): Sim = {
    sim
      .set(clock.out, true)
      .watch(clock.out, _.toggleAfter(clock.freq, clock.out))
  }

  def setupPosEdge(sim: Sim, posEdge: PosEdge): Sim = {
    sim
      .set(posEdge.out, false)
      .watch(
        posEdge.in,
        st => {
          st.get(posEdge.in) match {
            case Some(true) =>
              st.setAfter(GateDelay, posEdge.out, true)
                .setAfter(GateDelay + PosEdgeDelay, posEdge.out, false)
            case _ =>
              st
          }
        }
      )
  }

  def setupSwitch(sim: Sim, switch: Switch): Sim = {
    def propagate(s: Sim): Sim = {
      val res = (s.get(switch.enable), s.get(switch.in)) match {
        case (Some(true), v) => v
        case _ => None
      }
      s.set(switch.out, res)
    }
    sim.watch(switch.enable, propagate).watch(switch.in, propagate)
  }
}
