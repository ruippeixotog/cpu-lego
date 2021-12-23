package component

import component.BuilderDSL._
import core._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import simulator.{Circuit, PortChange, Sim, SimState}
import testkit._

class CoreSpec extends BaseSpec {

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

  "A Flipflop" should {

    "start unset" in {
      val ((q, nq), state) = buildAndRun { implicit env => flipflop(new Port, new Port) }
      state.get(q) must beNone
      state.get(nq) must beNone
    }

    "be set to High when S is set to High" in {
      val ((q, nq), state) = buildAndRun { implicit env => flipflop(High, Low) }
      state.get(q) must beSome(true)
      state.get(nq) must beSome(false)
    }

    "be set to Low when R is set to High" in {
      val ((q, nq), state) = buildAndRun { implicit env => flipflop(Low, High) }
      state.get(q) must beSome(false)
      state.get(nq) must beSome(true)
    }

    "retain its original value when both S and R are High" in {
      val set, reset = new Port
      val ((q, nq), comp) = buildComponent { implicit env => flipflop(set, reset) }

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
  }

  "A Clock" should {

    "start at High" in {
      val (out, comp) = buildComponent { implicit env => clock(100) }
      val state = Sim.runComponent(comp, Some(0))
      state.get(out) must beSome(true)
    }

    "toggle its value according to its frequency" in {
      forAll(Gen.choose(10, 1000), Gen.choose(10, 1000)) { (freq, simEnd) =>
        val (out, comp) = buildComponent { implicit env => clock(freq) }
        val state = Sim.runComponent(comp, Some(simEnd))
        state.get(out) must beSome((simEnd / freq) % 2 == 0)
      }
    }
  }

  "A PosEdge" should {

    "output Low when unchanged" in forAll { (in: LogicLevel) =>
      val (out, state) = buildAndRun { implicit env => posEdge(in) }
      state.get(out) must beSome(false)
    }

    "output High when the input changes from Low to High" in {
      val (out, comp) = buildComponent { implicit env => posEdge(clock(50)) }
      // expected delay from clock out to posEdge out
      val delay = Sim.WireDelay + Sim.GateDelay

      foreachTick(comp, 250) { (tick, state) =>
        // Positive edge triggering for clock(50) occurs at t=0,100,200...
        state.get(out) must beSome((tick - delay + 100) % 100 < Sim.PosEdgeDelay)
      }
    }
  }
}
