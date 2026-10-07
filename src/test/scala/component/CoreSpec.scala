package component

import component.BuilderAPI.*
import core.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import simulator.GateProcessor
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
      val sim = GateProcessor.setupAndRun(comp, Some(0))
      sim.get(out) must beSome(true)
    }

    "start at the given initial level" in {
      val (out, comp) = buildComponent { clock(100, initialLevel = false) }
      val sim = GateProcessor.setupAndRun(comp, Some(0))
      sim.get(out) must beSome(false)
    }

    "keep toggling from a Low initial level" in {
      val (out, comp) = buildComponent { clock(100, initialLevel = false) }
      val sim = GateProcessor.setupAndRun(comp, Some(100))
      sim.get(out) must beSome(true)
    }

    "toggle its value according to its half-period" in {
      forAll(Gen.choose(10, 1000), Gen.choose(10, 1000)) { (halfPeriod, simEnd) =>
        val (out, comp) = buildComponent { clock(halfPeriod) }
        val sim = GateProcessor.setupAndRun(comp, Some(simEnd))
        sim.get(out) must beSome((simEnd / halfPeriod) % 2 == 0)
      }
    }

    "derive its half-period from a target frequency" in {
      halfPeriodOf(clockHz(1e6)) must be_==(500)
      halfPeriodOf(clockHz(250e6)) must be_==(2)
      halfPeriodOf(clockHz(1e9)) must be_==(1) // 0.5 ticks, clamped to 1
    }

    "round the derived half-period to the nearest tick" in {
      // 1e9 / (2 * 3e8) = 1.667 ticks -> 2
      halfPeriodOf(clockHz(3e8)) must be_==(2)
      // the achieved frequency then differs from the target by at most half a tick per half-period
      val achieved = 1e9 / (2 * Clock.halfPeriodForHz(3e8))
      achieved must beCloseTo(3e8, 0.6e8)
    }

    "reject a non-positive target frequency" in {
      Clock.halfPeriodForHz(0) must throwA[IllegalArgumentException]
    }

    "toggle at the derived half-period" in {
      val (out, comp) = buildComponent { clockHz(250e6) } // half-period 2: t=0 High, t=2 Low
      val sim = GateProcessor.setupAndRun(comp, Some(3))
      sim.get(out) must beSome(false)
    }
  }

  private def halfPeriodOf(spec: Spec[Port]): Int = {
    val (_, comp) = buildComponent(spec)
    def find(c: Component): Option[Clock] = c match {
      case clk: Clock => Some(clk)
      case CompositeComponent(_, components, _, _) => components.values.view.flatMap(find).headOption
      case _ => None
    }
    find(comp).get.halfPeriod
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
