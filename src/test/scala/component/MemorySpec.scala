package component

import component.BuilderDSL._
import core._
import org.scalacheck.Prop.forAll
import simulator.{PortChange, Sim, SimState}

class MemorySpec extends util.BaseSpec {

  "A nandLatch" should {

    "start unset" in {
      val ((q, nq), state) = buildAndRun { implicit env => nandLatch(new Port, new Port) }
      state.get(q) must beNone
      state.get(nq) must beNone
    }

    "be set to High when S is set to High" in {
      val ((q, nq), state) = buildAndRun { implicit env => nandLatch(High, Low) }
      state.get(q) must beSome(true)
      state.get(nq) must beSome(false)
    }

    "be set to Low when R is set to High" in {
      val ((q, nq), state) = buildAndRun { implicit env => nandLatch(Low, High) }
      state.get(q) must beSome(false)
      state.get(nq) must beSome(true)
    }

    "retain its original value when both S and R are High" in {
      val set, reset = new Port
      val ((q, nq), comp) = buildComponent { implicit env => nandLatch(set, reset) }

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

  "A latchClocked" should {

    "start unset" in {
      val ((q, nq), comp) = buildComponent { implicit env => latchClocked(new Port, new Port, clock(100)) }
      val state = Sim.runComponent(comp, Some(1000))
      state.get(q) must beNone
      state.get(nq) must beNone
    }

    "remain unset while clk is Low" in {
      val ((q, nq), state) = buildAndRun { implicit env => latchClocked(High, Low, Low) }
      state.get(q) must beNone
      state.get(nq) must beNone
    }

    "be set to High when S is set to High and clk is High" in {
      val ((q, nq), state) = buildAndRun { implicit env => latchClocked(High, Low, High) }
      state.get(q) must beSome(true)
      state.get(nq) must beSome(false)
    }

    "be set unconditionally to High when preset is Low" in {
      val ((q, nq), state) = buildAndRun { implicit env => latchClocked(Low, High, Low, preset = Low) }
      state.get(q) must beSome(true)
      state.get(nq) must beSome(false)
    }

    "be set to Low when R is set to High and clk is High" in {
      val ((q, nq), state) = buildAndRun { implicit env => latchClocked(Low, High, High) }
      state.get(q) must beSome(false)
      state.get(nq) must beSome(true)
    }

    "be set unconditionally to Low when clear is Low" in {
      val ((q, nq), state) = buildAndRun { implicit env => latchClocked(High, Low, Low, clear = Low) }
      state.get(q) must beSome(false)
      state.get(nq) must beSome(true)
    }

    "retain its original value when both S and R are Low" in {
      val set, reset = new Port
      val ((q, nq), comp) = buildComponent { implicit env => latchClocked(set, reset, clock(100)) }

      def setInputs(s: Boolean, r: Boolean)(state: SimState) = {
        state.schedule(0, PortChange(set, Some(s)))
        state.schedule(0, PortChange(reset, Some(r)))
      }

      runPlan(
        comp,
        50 -> { _.get(q) must beNone },
        150 -> setInputs(false, false),
        250 -> { _.get(q) must beNone },
        350 -> setInputs(true, false),
        375 -> { _.get(q) must beNone },
        450 -> { _.get(q) must beSome(true) },
        550 -> setInputs(false, false),
        650 -> { _.get(q) must beSome(true) },
        750 -> setInputs(false, true),
        775 -> { _.get(q) must beSome(true) },
        850 -> { _.get(q) must beSome(false) }
      )
    }
  }

  "A dLatch" should {

    "start unset" in {
      val ((q, nq), comp) = buildComponent { implicit env => dLatch(new Port, clock(100)) }
      val state = Sim.runComponent(comp, Some(1000))
      state.get(q) must beNone
      state.get(nq) must beNone
    }

    "be set to the input on positive edge trigger" in forAll { (in: LogicLevel) =>
      val ((q, nq), comp) = buildComponent { implicit env => dLatch(in, clock(100)) }
      val state = Sim.runComponent(comp, Some(50))
      state.get(q) must beSome(in.toBool)
      state.get(nq) must beSome(!in.toBool)
    }

    "retain its original value outside positive edges" in {
      val in = new Port
      val ((q, nq), comp) = buildComponent { implicit env => dLatch(in, clock(100)) }

      def setInput(d: Boolean)(state: SimState) = {
        state.schedule(0, PortChange(in, Some(d)))
      }

      runPlan(
        comp,
        25 -> { _.get(q) must beNone },
        50 -> setInput(true),
        75 -> { _.get(q) must beNone },
        150 -> { _.get(q) must beNone },
        250 -> { _.get(q) must beSome(true) },

        300 -> setInput(false),
        350 -> { _.get(q) must beSome(true) },
        450 -> { _.get(q) must beSome(false) }
      )
    }
  }
}
