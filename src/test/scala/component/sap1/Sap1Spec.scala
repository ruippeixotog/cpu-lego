package component.sap1

import component.*
import component.BuilderAPI.*
import component.sap1.ControlBus.Bit.*
import core.*
import org.specs2.specification.core.Fragment
import testkit.*
import util.Implicits.*

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

      var expectedReg = Vector.fill(4)(Some(false)) ++ Vector.fill(4)(None)

      SequentialScenario(comp)
        .withPorts(load -> false, clk -> true, clr -> false, enable -> true, ins -> false)
        .onStart { _ =>
          expectedReg = Vector.fill(4)(Some(false)) ++ Vector.fill(4)(None)
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
          expectedReg = Vector.fill(4)(Some(false)) ++ expectedReg.drop(4)
        }
        .check { sim =>
          sim.get(bus.drop(4)) aka "the bus" must beEqualTo(
            if (sim.isHigh(enable)) expectedReg.drop(4)
            else if (sim.isHigh(load)) sim.get(ins.drop(4))
            else Vector.fill(4)(None)
          )
          sim.get(instr) aka "the instruction" must beEqualTo(expectedReg.take(4))
        }
        .run()
    }
  }

  "An input, MAR and RAM" should {

    "work as intended" in {
      val ins, addrIn = newBus(4)
      val bus, dataIn = newBus(8)
      val prog, write, load, clk, enable = newPort()
      val ramIn = Input(prog, write, addrIn, dataIn)

      val (_, comp) = buildComponent {
        buffered(load)(ins) ~> bus.drop(4)
        val mOut = inputAndMar(bus, load, ramIn, clk)
        sap1.ram(bus, mOut, enable, ramIn)
      }

      var mem: Array[Vector[Option[Boolean]]] = Array.fill(16)(Vector.fill(8)(None))
      var addrReg: Vector[Option[Boolean]] = Vector.fill(4)(None)

      SequentialScenario(comp)
        .withPorts(prog -> true, write -> false, addrIn -> false, dataIn -> false)
        .withPorts(ins -> false, load -> true, clk -> true, enable -> false)
        .onStart { _ =>
          mem = Array.fill(16)(Vector.fill(8)(None))
          addrReg = Vector.fill(4)(None)
        }
        .beforeAction {
          // ensure `enable` and `load` are not High at the same time
          case (sim, `enable`, true, _) => sim.set(load, false)
          case (sim, `load`, true, _) => sim.set(enable, false)
          case (sim, _, _, _) => sim
        }
        .onPosEdge(clk) { sim =>
          if (sim.isHigh(load)) {
            addrReg = sim.get(bus.drop(4))
          }
        }
        .whenHigh(write) { sim =>
          val addr = if (sim.isHigh(prog)) sim.get(addrIn) else addrReg
          addr.sequence.foreach { addr0 =>
            mem(addr0.toInt) = sim.get(dataIn)
          }
        }
        .check { sim =>
          if (sim.isHigh(enable) && sim.isLow(prog)) {
            addrReg.sequence match {
              case Some(addr) => sim.get(bus) must beEqualTo(mem(addr.toInt))
              case None => sim.get(bus) must beEqualTo(Vector.fill(8)(None))
            }
          }
          ok
        }
        .run()
    }
  }
}
