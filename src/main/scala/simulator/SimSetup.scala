package simulator

import core._

object SimSetup {
  val GateDelay = 1
  val PosEdgeDelay = 1

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
}