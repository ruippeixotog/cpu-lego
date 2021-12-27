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
      xs <- Gen.listOfN(n, summon[Arbitrary[LogicLevel]].arbitrary)
    } yield xs.toVector
  )

  "A decoder" should {

    "select a single output based on the combination of inputs" in {
      forAll { (ins: Vector[LogicLevel], enable: LogicLevel) =>
        val (outs, state) = buildAndRun { decoder(ins, enable) }
        outs must haveLength(1 << ins.length)

        val expectedIdx = ins.toInt
        foreach(outs.zipWithIndex) { (out, idx) =>
          state.get(out) must beEqualTo(Some(enable.toBool && idx == expectedIdx))
        }
      }
    }
  }
}
