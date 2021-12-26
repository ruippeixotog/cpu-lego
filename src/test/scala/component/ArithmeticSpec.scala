package component

import component.BuilderAPI._
import core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import simulator.{Circuit, Sim}
import testkit._

class ArithmeticSpec extends BaseSpec {

  given Arbitrary[List[(LogicLevel, LogicLevel)]] = Arbitrary(
    Gen.listOfN(20, summon[Arbitrary[(LogicLevel, LogicLevel)]].arbitrary)
  )

  "A fullAdder" should {
    "compute an addition with carry correctly" in forAll { (in1: LogicLevel, in2: LogicLevel, in3: LogicLevel) =>
      val ((out, carry), state) = buildAndRun { fullAdder(in1, in2, in3) }

      val expected = List(in1, in2, in3).map(_.toInt).sum
      state.get(out) must beSome(expected % 2 == 1)
      state.get(carry) must beSome(expected / 2 == 1)
    }
  }

  "A binaryAdder" should {
    "compute an addition correctly" in forAll { (ins: List[(LogicLevel, LogicLevel)]) =>
      val (in1, in2) = ins.unzip
      val ((outs, carry), state) = buildAndRun { binaryAdder(in1, in2) }

      (outs :+ carry).map(state.get).sequence must beSome.which { bools =>
        bools.toInt must beEqualTo(in1.toInt + in2.toInt)
      }
    }
  }

  "An addSub" should {

    "compute an addition correctly" in forAll { (ins: List[(LogicLevel, LogicLevel)]) =>
      val n = ins.length
      val (in1, in2) = ins.unzip
      val (outs, state) = buildAndRun { addSub(in1, in2, Low) }

      outs.map(state.get).sequence must beSome.which { bools =>
        bools.toInt.truncate(n) must beEqualTo((in1.toInt + in2.toInt).truncate(n))
      }
    }

    "compute a subtraction correctly" in forAll { (ins: List[(LogicLevel, LogicLevel)]) =>
      val n = ins.length
      val (in1, in2) = ins.unzip
      val (outs, state) = buildAndRun { addSub(in1, in2, High) }

      outs.map(state.get).sequence must beSome.which { bools =>
        bools.toInt.truncate(n) must beEqualTo((in1.toInt - in2.toInt).truncate(n))
      }
    }
  }
}
