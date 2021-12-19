package component

import component.BuilderDSL._
import core._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import simulator.{Circuit, Sim}

class CoreSpec extends util.BaseSpec {

  "A NAND" should {
    "compute !(a & b)" in forAll { (in1: Option[LogicLevel], in2: Option[LogicLevel]) =>
      val expected = (in1, in2) match {
        case (Some(Low), _) => Some(true)
        case (_, Some(Low)) => Some(true)
        case (Some(High), Some(High)) => Some(false)
        case _ => None
      }
      val (out, state) = buildAndRun { implicit env => nand(in1.toPort, in2.toPort) }
      state.get(out) must beEqualTo(expected)
    }
  }

  "A Clock" should {
    "start at Low" in {
      val (out, comp) = buildComponent { implicit env => clock(100) }
      val state = Sim.runComponent(comp, Some(0))
      state.get(out) must beSome(false)
    }

    "toggle its value according to its frequency" in {
      forAll(Gen.choose(10, 1000), Gen.choose(10, 1000)) { (freq, simEnd) =>
        val (out, comp) = buildComponent { implicit env => clock(freq) }
        val state = Sim.runComponent(comp, Some(simEnd))
        state.get(out) must beSome((simEnd / freq) % 2 == 1)
      }
    }
  }
}
