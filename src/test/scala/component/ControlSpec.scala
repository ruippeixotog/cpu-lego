package component

import component.BuilderAPI._
import core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import testkit._

class ControlSpec extends BaseSpec {

  given Arbitrary[Vector[LogicLevel]] = Arbitrary(
    for {
      n <- Gen.choose(1, 5)
      xs <- Gen.listOfN(n, genLogicLevel)
    } yield xs.toVector
  )

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
}
