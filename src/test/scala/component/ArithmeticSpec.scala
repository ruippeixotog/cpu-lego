package component

import component.BuilderDSL._
import core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import simulator.{Circuit, Sim}

class ArithmeticSpec extends util.BaseSpec {

  given Arbitrary[List[(LogicLevel, LogicLevel)]] = Arbitrary(
    Gen.listOfN(20, summon[Arbitrary[(LogicLevel, LogicLevel)]].arbitrary)
  )

  "A fullAdder" should {
    "compute an addition with carry correctly" in forAll { (sig1: LogicLevel, sig2: LogicLevel, sig3: LogicLevel) =>
      val ((out, carry), comp) = buildComponent { implicit env => fullAdder(sig1, sig2, sig3) }
      val state = Sim.runComponent(comp)
      
      val expected = List(sig1, sig2, sig3).map(_.toInt).sum

      state.get(out) must beSome(expected % 2 == 1)
      state.get(carry) must beSome(expected / 2 == 1)
    }
  }

  "A binaryAdder" should {
    "compute an addition correctly" in forAll { (sigs: List[(LogicLevel, LogicLevel)]) =>
      val (sigs1, sigs2) = sigs.unzip
      val ((outs, carry), comp) = buildComponent { implicit env => binaryAdder(sigs1, sigs2) }
      val state = Sim.runComponent(comp)

      val outSigs = outs :+ carry
      outSigs.map(state.get) must not(contain(None))
      outSigs.flatMap(state.get).toUInt must beEqualTo(sigs1.toUInt + sigs2.toUInt)
    }
  }

  "An addSub" should {
    "compute an addition correctly" in forAll { (sigs: List[(LogicLevel, LogicLevel)]) =>
      val (sigs1, sigs2) = sigs.unzip
      val (outs, comp) = buildComponent { implicit env => addSub(sigs1, sigs2, Low) }
      val state = Sim.runComponent(comp)

      outs.map(state.get) must not(contain(None))
      outs.flatMap(state.get).toInt must beEqualTo(sigs1.toInt + sigs2.toInt)
    }
    "compute a subtraction correctly" in forAll { (sigs: List[(LogicLevel, LogicLevel)]) =>
      val (sigs1, sigs2) = sigs.unzip
      val (outs, comp) = buildComponent { implicit env => addSub(sigs1, sigs2, High) }
      val state = Sim.runComponent(comp)
      
      outs.map(state.get) must not(contain(None))
      outs.flatMap(state.get).toInt must beEqualTo(sigs1.toInt - sigs2.toInt)
    }
  }
}
