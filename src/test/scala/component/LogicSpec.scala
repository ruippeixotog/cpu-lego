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
    "compute !a" in forAll { (sig: LogicLevel) =>
      val (out, comp) = buildComponent { implicit env => component.not(sig) }
      val state = Sim.runComponent(comp)
      state.get(out) must beSome(!sig.toBool)
    }
  }

  "An AND" should {
    "compute a & b" in forAll { (sig1: LogicLevel, sig2: LogicLevel) =>
      val (out, comp) = buildComponent { implicit env => and(sig1, sig2) }
      val state = Sim.runComponent(comp)
      state.get(out) must beSome(sig1.toBool && sig2.toBool)
    }
  }

  "An OR" should {
    "compute a | b" in forAll { (sig1: LogicLevel, sig2: LogicLevel) =>
      val (out, comp) = buildComponent { implicit env => or(sig1, sig2) }
      val state = Sim.runComponent(comp)
      state.get(out) must beSome(sig1.toBool || sig2.toBool)
    }
  }

  "A NOR" should {
    "compute !(a | b)" in forAll { (sig1: LogicLevel, sig2: LogicLevel) =>
      val (out, comp) = buildComponent { implicit env => nor(sig1, sig2) }
      val state = Sim.runComponent(comp)
      state.get(out) must beSome(!(sig1.toBool || sig2.toBool))
    }
  }

  "A XOR" should {
    "compute a ^ b" in forAll { (sig1: LogicLevel, sig2: LogicLevel) =>
      val (out, comp) = buildComponent { implicit env => xor(sig1, sig2) }
      val state = Sim.runComponent(comp)
      state.get(out) must beSome(sig1.toBool ^ sig2.toBool)
    }
  }

  "A XNOR" should {
    "compute !(a ^ b)" in forAll { (sig1: LogicLevel, sig2: LogicLevel) =>
      val (out, comp) = buildComponent { implicit env => xnor(sig1, sig2) }
      val state = Sim.runComponent(comp)
      state.get(out) must beSome(!(sig1.toBool ^ sig2.toBool))
    }
  }
}
