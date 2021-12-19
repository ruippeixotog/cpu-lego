package component

import component.BuilderDSL._
import core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import simulator.{Circuit, Sim}

class LogicSpec extends util.BaseSpec {

  "A NOT" should {
    "compute !a" in forAll { (in: LogicLevel) =>
      val (out, state) = buildAndRun { implicit env => component.not(in) }
      state.get(out) must beSome(!in.toBool)
    }
  }

  "An AND" should {
    "compute a & b" in forAll { (in1: LogicLevel, in2: LogicLevel) =>
      val (out, state) = buildAndRun { implicit env => and(in1, in2) }
      state.get(out) must beSome(in1.toBool && in2.toBool)
    }
  }

  "An OR" should {
    "compute a | b" in forAll { (in1: LogicLevel, in2: LogicLevel) =>
      val (out, state) = buildAndRun { implicit env => or(in1, in2) }
      state.get(out) must beSome(in1.toBool || in2.toBool)
    }
  }

  "A NOR" should {
    "compute !(a | b)" in forAll { (in1: LogicLevel, in2: LogicLevel) =>
      val (out, state) = buildAndRun { implicit env => nor(in1, in2) }
      state.get(out) must beSome(!(in1.toBool || in2.toBool))
    }
  }

  "A XOR" should {
    "compute a ^ b" in forAll { (in1: LogicLevel, in2: LogicLevel) =>
      val (out, state) = buildAndRun { implicit env => xor(in1, in2) }
      state.get(out) must beSome(in1.toBool ^ in2.toBool)
    }
  }

  "A XNOR" should {
    "compute !(a ^ b)" in forAll { (in1: LogicLevel, in2: LogicLevel) =>
      val (out, state) = buildAndRun { implicit env => xnor(in1, in2) }
      state.get(out) must beSome(!(in1.toBool ^ in2.toBool))
    }
  }
}
