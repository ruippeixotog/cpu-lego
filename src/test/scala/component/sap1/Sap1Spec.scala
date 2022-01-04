package component.sap1

import component._
import component.BuilderAPI._
import core._
import testkit._
import simulator.PortChange

class Sap1Spec extends BaseSpec with SequentialScenarios {

  "An instruction register" should {

    "work as intended" in {
      val bus, ins = newBus(8)
      val load, clk, clr, enable = newPort()
      val (instr, comp) = buildComponent {
        buffered(load)(ins) ~> bus
        instrRegister(bus, load, clk, clr, enable)
      }

      var expectedReg = Vector.fill(4)(None) ++ Vector.fill(4)(Some(false))

      SequentialScenario(comp)
        .withPorts(load -> false, clk -> true, clr -> false, enable -> true, ins -> false)
        .onStart { _ =>
          expectedReg = Vector.fill(4)(None) ++ Vector.fill(4)(Some(false))
        }
        .beforeAction {
          // ensure `enable` and `load` are not High at the same time
          case (state, `enable`, true, _) => state.schedule(0, PortChange(load, Some(false)))
          case (state, `load`, true, _) => state.schedule(0, PortChange(enable, Some(false)))
          case (state, _, _, _) => state
        }
        .onPosEdge(clk) { state =>
          if (state.get(load) == Some(true)) {
            expectedReg = ins.map(state.get)
          }
        }
        .whenLow(clr) { _ =>
          expectedReg = expectedReg.slice(0, 4) ++ Vector.fill(4)(Some(false))
        }
        .check { state =>
          bus.slice(0, 4).map(state.get) aka "the bus" must beEqualTo(
            if (state.get(enable) == Some(true)) expectedReg.slice(0, 4)
            else if (state.get(load) == Some(true)) ins.slice(0, 4).map(state.get)
            else Vector.fill(4)(None)
          )
          instr.map(state.get) aka "the instruction" must beEqualTo(expectedReg.slice(4, 8))
        }
        .run()
    }
  }
}
