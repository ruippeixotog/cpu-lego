package component.sap1

import component.BuilderAPI._
import component._
import component.sap1.ControlBus.Bit._
import core._
import org.specs2.specification.core.Fragment
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
        val (ControlBus(con), comp) = buildComponent { sequencer(instr, clk, clr) }

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
          .check { sim => sim.get(con).sequence must beSome(expectedCon(t)) }
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
          case (sim, `enable`, true, _) => sim.set(load, false)
          case (sim, `load`, true, _) => sim.set(enable, false)
          case (sim, _, _, _) => sim
        }
        .onPosEdge(clk) { sim =>
          if (sim.isHigh(load)) {
            expectedReg = sim.get(ins)
          }
        }
        .whenLow(clr) { _ =>
          expectedReg = expectedReg.slice(0, 4) ++ Vector.fill(4)(Some(false))
        }
        .check { sim =>
          sim.get(bus.slice(0, 4)) aka "the bus" must beEqualTo(
            if (sim.isHigh(enable)) expectedReg.slice(0, 4)
            else if (sim.isHigh(load)) sim.get(ins.slice(0, 4))
            else Vector.fill(4)(None)
          )
          sim.get(instr) aka "the instruction" must beEqualTo(expectedReg.slice(4, 8))
        }
        .run()
    }
  }
}
