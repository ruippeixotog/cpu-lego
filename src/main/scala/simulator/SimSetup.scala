package simulator

import core._

object SimSetup {

  def setup(c: Circuit): Sim = {
    val sim = Sim(c).set(High, true).set(Low, false)
    import sim.conf._

    c.components.foldLeft(sim) {

      case (sim, NAND(in1, in2, out)) =>
        binaryOp(sim, in1, in2, out, gateDelay) {
          case (Some(false), _) => Some(true)
          case (_, Some(false)) => Some(true)
          case (Some(true), Some(true)) => Some(false)
          case _ => None
        }

      case (sim, FlipFlop(set, reset, q, nq)) =>
        def propagate(s: Sim): Sim = {
          val res = (s.get(set), s.get(reset)) match {
            case (Some(true), Some(false)) => Some(true)
            case (Some(false), Some(true)) => Some(false)
            case _ => None
          }
          res.fold(s) { v =>
            s.setAfter(gateDelay, q, v).setAfter(gateDelay, nq, !v)
          }
        }
        sim.watch(set)(propagate).watch(reset)(propagate)

      case (sim, Clock(freq, out)) =>
        sim.set(out, true).watch(out)(_.toggleAfter(freq, out))

      case (sim, PosEdge(in, out)) =>
        sim
          .set(out, false)
          .watch(in) { st =>
            st.get(in) match {
              case Some(true) =>
                st.setAfter(gateDelay, out, true)
                  .setAfter(gateDelay + posEdgeDuration, out, false)
              case _ =>
                st
            }
          }

      case (sim, Switch(in, out, enable)) =>
        binaryOp(sim, enable, in, out, 0) {
          case (Some(true), v) => v
          case _ => None
        }
    }
  }

  private def binaryOp(sim: Sim, port1: Port, port2: Port, out: Port, delay: Int)(
      f: (Option[Boolean], Option[Boolean]) => Option[Boolean]
  ): Sim = {

    def propagate(s: Sim): Sim =
      s.setAfter(delay, out, f(s.get(port1), s.get(port2)))

    sim.watch(port1)(propagate).watch(port2)(propagate)
  }
}
