package component

import component.BuilderAPI._
import core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import simulator.{Circuit, Sim}
import testkit._
import util.Implicits._

class ArithmeticSpec extends BaseSpec {

  given Arbitrary[Vector[(LogicLevel, LogicLevel)]] = Arbitrary(
    for {
      n <- Gen.choose(1, 20)
      xs <- Gen.listOfN(n, summon[Arbitrary[(LogicLevel, LogicLevel)]].arbitrary)
    } yield xs.toVector
  )

  "A fullAdder" should {
    "compute an addition with carry correctly" in forAll { (in1: LogicLevel, in2: LogicLevel, in3: LogicLevel) =>
      val ((out, carry), sim) = buildAndRun { fullAdder(in1, in2, in3) }

      val expected = List(in1, in2, in3).map(_.toInt).sum
      sim.get(out) must beSome(expected % 2 == 1)
      sim.get(carry) must beSome(expected / 2 == 1)
    }
  }

  "A binaryAdder" should {
    "compute an addition correctly" in forAll { (ins: Vector[(LogicLevel, LogicLevel)]) =>
      val (in1, in2) = ins.unzip
      val ((outs, carry), sim) = buildAndRun { binaryAdder(in1, in2) }

      sim.get(outs :+ carry).sequence must beSome.which { bools =>
        bools.toInt must beEqualTo(in1.toInt + in2.toInt)
      }
    }
  }

  "An addSub" should {

    "compute an addition correctly" in forAll { (ins: Vector[(LogicLevel, LogicLevel)]) =>
      val n = ins.length
      val (in1, in2) = ins.unzip
      val (outs, sim) = buildAndRun { addSub(in1, in2, Low) }

      sim.get(outs).sequence must beSome.which { bools =>
        bools.toInt.truncate(n) must beEqualTo((in1.toInt + in2.toInt).truncate(n))
      }
    }

    "compute a subtraction correctly" in forAll { (ins: Vector[(LogicLevel, LogicLevel)]) =>
      val n = ins.length
      val (in1, in2) = ins.unzip
      val (outs, sim) = buildAndRun { addSub(in1, in2, High) }

      sim.get(outs).sequence must beSome.which { bools =>
        bools.toInt.truncate(n) must beEqualTo((in1.toInt - in2.toInt).truncate(n))
      }
    }
  }
}
