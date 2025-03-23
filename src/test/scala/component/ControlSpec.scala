package component

import component.BuilderAPI.*
import core.*
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import testkit.*
import util.Implicits.*

class ControlSpec extends BaseSpec {

  given Arbitrary[Vector[LogicLevel]] = Arbitrary(
    for {
      n <- Gen.choose(1, 5)
      xs <- Gen.listOfN(n, genLogicLevel)
    } yield xs.toVector
  )

  "A posEdge" should {

    "output Low when unchanged" in forAll { (in: LogicLevel) =>
      val (out, sim) = buildAndRun { posEdge(in) }
      sim.get(out) must beSome(false)
    }

    "output High when the input changes from Low to High" in {
      val (out, comp) = buildComponent { posEdge(clock(50)) }

      foreachTick(comp, 25, 250) { (tick, sim) =>
        // Positive edge triggering for clock(50) occurs at t=0,100,200...
        // implementation-specific check - two NAND gates used
        val delay = sim.conf.wireDelay * 2 + sim.conf.gateDelay * 2
        val duration = 2
        sim.get(out) must beSome((tick - delay + 100) % 100 < duration)
      }
    }
  }

  "A negEdge" should {

    "output Low when unchanged" in forAll { (in: LogicLevel) =>
      val (out, sim) = buildAndRun { negEdge(in) }
      sim.get(out) must beSome(false)
    }

    "output High when the input changes from High to Low" in {
      val (out, comp) = buildComponent { negEdge(clock(50)) }

      foreachTick(comp, 25, 250) { (tick, sim) =>
        // Negative edge triggering for clock(50) occurs at t=50,150...
        // implementation-specific check - three NAND gates used
        val delay = sim.conf.wireDelay * 3 + sim.conf.gateDelay * 3
        val duration = 2
        sim.get(out) must beSome((tick - 50 - delay + 100) % 100 < duration)
      }
    }
  }

  "A decoder" should {

    "select a single output based on the combination of inputs" in {
      forAll { (ins: Vector[LogicLevel], enable: LogicLevel) =>
        val (outs, sim) = buildAndRun { decoder(ins, enable) }
        outs must haveLength(1 << ins.length)

        val expectedIdx = ins.toInt
        foreach(outs.zipWithIndex) { (out, idx) =>
          sim.get(out) must beEqualTo(Some(enable.toBool && idx == expectedIdx))
        }
      }
    }
  }

  "A mux" should {

    "act as an N-to-1 multiplexer" in {
      forAll { (sel: Vector[LogicLevel]) =>
        forAll(Gen.listOfN(1 << sel.length, genLogicLevel).map(_.toVector)) { ins =>
          val (out, sim) = buildAndRun { mux(ins, sel) }
          sim.get(out) must beSome(ins(sel.toInt).toBool)
        }
      }
    }

    "throw when the input bus size and the address size do not match" in {
      val ins, sel = newBus(3)
      buildAndRun { mux(ins, sel) } must throwAn[AssertionError]
    }
  }

  "A muxN" should {

    "act as an M-to-N multiplexer" in {
      forAll { (sel: Vector[LogicLevel]) =>
        forAll(Gen.choose(1, 4)) { width =>
          forAll(Gen.listOfN((1 << sel.length) * width, genLogicLevel).map(_.toVector)) { ins =>
            val (out, sim) = buildAndRun { muxN(ins, sel, width) }
            sim.get(out).sequence must beSome(ins.grouped(width).toVector(sel.toInt).map(_.toBool))
          }
        }
      }
    }

    "throw when the input bus size and the address size do not match" in {
      val ins, sel = newBus(3)
      buildAndRun { muxN(ins, sel, 2) } must throwAn[AssertionError]
    }
  }
}
