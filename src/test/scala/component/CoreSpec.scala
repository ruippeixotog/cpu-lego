package component

import component.BuilderAPI._
import core._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import simulator.{Circuit, PortChange, Sim, SimState}
import testkit._

class CoreSpec extends BaseSpec with SequentialScenarios {

  "A NAND" should {

    "compute !(a & b)" in forAll { (in1: Option[LogicLevel], in2: Option[LogicLevel]) =>
      val expected = (in1, in2) match {
        case (Some(Low), _) => Some(true)
        case (_, Some(Low)) => Some(true)
        case (Some(High), Some(High)) => Some(false)
        case _ => None
      }
      val (out, state) = buildAndRun { nand(in1.toPort, in2.toPort) }
      state.get(out) must beEqualTo(expected)
    }
  }

  "A Flipflop" should {

    "start unset" in {
      val ((q, nq), state) = buildAndRun { flipflop(new Port, new Port) }
      state.get(q) must beNone
      state.get(nq) must beNone
    }

    "be set to High when S is set to High" in {
      val ((q, nq), state) = buildAndRun { flipflop(High, Low) }
      state.get(q) must beSome(true)
      state.get(nq) must beSome(false)
    }

    "be set to Low when R is set to High" in {
      val ((q, nq), state) = buildAndRun { flipflop(Low, High) }
      state.get(q) must beSome(false)
      state.get(nq) must beSome(true)
    }

    "retain its original value when both S and R are High" in {
      val set, reset = newPort()
      val ((q, nq), comp) = buildComponent { flipflop(set, reset) }

      def setInputs(s: Boolean, r: Boolean)(state: SimState): Unit = {
        state.schedule(0, PortChange(set, Some(s)))
        state.schedule(0, PortChange(reset, Some(r)))
      }

      runPlan(
        comp,
        10 -> { _.get(q) must beNone },
        20 -> setInputs(true, true),
        30 -> { _.get(q) must beNone },
        40 -> setInputs(true, false),
        50 -> { _.get(q) must beSome(true) },
        60 -> setInputs(true, true),
        70 -> { _.get(q) must beSome(true) },
        80 -> setInputs(false, true),
        90 -> { _.get(q) must beSome(false) },
        100 -> setInputs(true, true),
        110 -> { _.get(q) must beSome(false) }
      )
    }

    "behave well under any combination of the above" in {
      val set, reset = newPort()
      val ((q, nq), comp) = buildComponent { flipflop(set, reset) }

      var expectedQ = Option.empty[Boolean]

      SequentialScenario(comp)
        .withPorts(set -> None, reset -> None)
        .onStart { _ => expectedQ = None }
        .beforeAction {
          // ensure `set` and `reset` are not High at the same time
          case (state, `set`, true, _) => state.schedule(0, PortChange(reset, Some(false)))
          case (state, `reset`, true, _) => state.schedule(0, PortChange(set, Some(false)))
          case _ => // do nothing
        }
        .onAction { (state, _, _, _) =>
          expectedQ = (state.get(set), state.get(reset)) match {
            case (Some(true), _) => Some(true)
            case (_, Some(true)) => Some(false)
            case _ => expectedQ
          }
        }
        .check { state =>
          state.get(q) must beEqualTo(expectedQ)
          state.get(nq) must beEqualTo(expectedQ.map(!_))
        }
        .run()
    }
  }

  "A Clock" should {

    "start at High" in {
      val (out, comp) = buildComponent { clock(100) }
      val state = Sim.runComponent(comp, Some(0))
      state.get(out) must beSome(true)
    }

    "toggle its value according to its frequency" in {
      forAll(Gen.choose(10, 1000), Gen.choose(10, 1000)) { (freq, simEnd) =>
        val (out, comp) = buildComponent { clock(freq) }
        val state = Sim.runComponent(comp, Some(simEnd))
        state.get(out) must beSome((simEnd / freq) % 2 == 0)
      }
    }
  }

  "A PosEdge" should {

    "output Low when unchanged" in forAll { (in: LogicLevel) =>
      val (out, state) = buildAndRun { posEdge(in) }
      state.get(out) must beSome(false)
    }

    "output High when the input changes from Low to High" in {
      val (out, comp) = buildComponent { posEdge(clock(50)) }
      // expected delay from clock out to posEdge out
      val delay = Sim.WireDelay + Sim.GateDelay

      foreachTick(comp, 250) { (tick, state) =>
        // Positive edge triggering for clock(50) occurs at t=0,100,200...
        state.get(out) must beSome((tick - delay + 100) % 100 < Sim.PosEdgeDelay)
      }
    }
  }

  "A Switch" should {

    "behave as a controlled switch" in forAll { (in: Option[LogicLevel], enable: LogicLevel) =>
      val (out, state) = buildAndRun { switch(in.toPort, enable) }
      state.get(out) must beEqualTo(if (enable == High) in.map(_.toBool) else None)
    }

    "behave well under any port change sequence" in {
      val in, enable = newPort()
      val (out, comp) = buildComponent { switch(in, enable) }

      SequentialScenario(comp)
        .withPorts(in -> None, enable -> Some(false))
        .check { state =>
          state.get(out) must beEqualTo(
            if (state.get(enable) == Some(true)) state.get(in) else None
          )
        }
        .run()
    }
  }
}
