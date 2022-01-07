package component.sap1

import component.BuilderAPI._
import component._
import component.sap1.ControlBus.Bit._
import core._
import org.specs2.specification.core.Fragment
import simulator.PortChange
import testkit._

class Sap1Spec extends BaseSpec with SequentialScenarios {

  "A sequencer" should {

    val steps = List(
      "LDA" -> (0, List(Lm, Ei), List(Ce, La), Nil),
      "ADD" -> (1, List(Lm, Ei), List(Ce, Lb), List(La, Eu)),
      "SUB" -> (2, List(Lm, Ei), List(Ce, Lb), List(La, Su, Eu)),
      "OUT" -> (14, List(Ea, Lo), Nil, Nil)
    )

    Fragment.foreach(steps) { case (op, (opcode, c4, c5, c6)) =>
      s"emit the correct words on a $op instruction" in {
        val instr = (0 until 4).map { i => if ((opcode & (1 << i)) == 0) Low else High }.toVector
        val clk, clr = newPort()
        val (con, comp) = buildComponent { sequencer(instr, clk, clr) }

        var t = 0
        val expectedCon = Vector(
          ControlBus.fromBits(Ep, Lm),
          ControlBus.fromBits(Cp),
          ControlBus.fromBits(Ce, Li)
        ) ++ Vector(c4, c5, c6).map(ControlBus.fromBits)

        SequentialScenario(comp)
          .withPorts(clk -> true, clr -> false)
          .onStart { _ => t = 0 }
          .onNegEdge(clk) { _ => t = (t + 1) % 6 }
          .whenLow(clr) { _ => t = 0 }
          .check { state => con.bus.map(state.get).sequence must beSome(expectedCon(t)) }
          .run()
      }
    }
  }

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
