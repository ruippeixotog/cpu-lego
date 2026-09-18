package component

import component.BuilderAPI.*
import core.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import simulator.{Circuit, Sim, SimSetup}
import testkit.*
import util.Implicits.*

class CoreSpec extends BaseSpec with SequentialScenarios {

  "A NAND" should {

    "compute !(a & b)" in forAll { (in1: Option[LogicLevel], in2: Option[LogicLevel]) =>
      val expected = (in1, in2) match {
        case (Some(Low), _) => Some(true)
        case (_, Some(Low)) => Some(true)
        case (Some(High), Some(High)) => Some(false)
        case _ => None
      }
      val (out, sim) = buildAndRun { nand(in1.toPort, in2.toPort) }
      sim.get(out) must beEqualTo(expected)
    }
  }

  "A Clock" should {

    "start at High" in {
      val (out, comp) = buildComponent { clock(100) }
      val sim = Sim.setupAndRun(comp, Some(0))
      sim.get(out) must beSome(true)
    }

    "toggle its value according to its frequency" in {
      forAll(Gen.choose(10, 1000), Gen.choose(10, 1000)) { (freq, simEnd) =>
        val (out, comp) = buildComponent { clock(freq) }
        val sim = Sim.setupAndRun(comp, Some(simEnd))
        sim.get(out) must beSome((simEnd / freq) % 2 == 0)
      }
    }
  }

  "A Switch" should {

    "behave as a controlled switch" in forAll { (in: Option[LogicLevel], enable: LogicLevel) =>
      val (out, sim) = buildAndRun { switch(in.toPort, enable) }
      sim.get(out) must beEqualTo(if (enable == High) in.map(_.toBool) else None)
    }

    "behave well under any port change sequence" in {
      val in, enable = newPort()
      val (out, comp) = buildComponent { switch(in, enable) }

      SequentialScenario(comp)
        .withPorts(in, enable -> false)
        .check { sim =>
          sim.get(out) must beEqualTo(
            if (sim.isHigh(enable)) sim.get(in) else None
          )
        }
        .run()
    }

    "let the output float when enable is unset" in forAll { (in: Option[LogicLevel]) =>
      val (out, sim) = buildAndRun { switch(in.toPort, new Port) }
      sim.get(out) must beNone
    }
  }
}
