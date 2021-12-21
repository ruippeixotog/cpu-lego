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
      val ((q, nq), state1) = buildAndRun { implicit env => nandLatch(set, reset) }
      state1.get(q) must beNone

      def setAndRun(state: SimState, s: Boolean, r: Boolean): SimState = {
        state.schedule(0, PortChange(set, Some(s)))
        state.schedule(0, PortChange(reset, Some(r)))
        Sim.run(state)
      }

      val state2 = setAndRun(state1, true, true)
      state2.get(q) must beNone
      val state3 = setAndRun(state2, true, false)
      state3.get(q) must beSome(true)
      val state4 = setAndRun(state3, true, true)
      state4.get(q) must beSome(true)
      val state5 = setAndRun(state4, false, true)
      state5.get(q) must beSome(false)
      val state6 = setAndRun(state5, true, true)
      state6.get(q) must beSome(false)
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
      var state = Sim.runComponent(comp, Some(50))
      state.get(q) must beNone

      def setInputs(tick: Int, s: Boolean, r: Boolean) = {
        state.schedule(tick - state.t, PortChange(set, Some(s)))
        state.schedule(tick - state.t, PortChange(reset, Some(r)))
      }
      def resume(tick: Int) = state = Sim.run(state, Some(tick))

      setInputs(150, false, false)
      resume(250)
      state.get(q) must beNone

      setInputs(350, true, false)
      resume(350)
      state.get(q) must beNone
      resume(450)
      state.get(q) must beSome(true)

      setInputs(550, false, false)
      resume(650)
      state.get(q) must beSome(true)

      setInputs(750, false, true)
      resume(750)
      state.get(q) must beSome(true)
      resume(850)
      state.get(q) must beSome(false)
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
      var state = Sim.runComponent(comp, Some(50))
      state.get(q) must beNone

      def setInputs(tick: Int, d: Boolean) = {
        state.schedule(tick - state.t, PortChange(in, Some(d)))
      }
      def resume(tick: Int) = state = Sim.run(state, Some(tick))

      setInputs(50, true)
      resume(50)
      state.get(q) must beNone
      resume(150)
      state.get(q) must beNone
      resume(250)
      state.get(q) must beSome(true)

      setInputs(300, false)
      resume(350)
      state.get(q) must beSome(true)
      resume(450)
      state.get(q) must beSome(false)
    }
  }
}
