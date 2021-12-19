package component

import core._
import simulator.{PortChange, Sim, SimState}

class MemorySpec extends util.BaseSpec {

  "A norLatch" should {

    "start unset" in {
      val ((q, nq), state) = buildAndRun { implicit env => norLatch(new Port, new Port) }
      state.get(q) must beNone
      state.get(nq) must beNone
    }

    "be set to High when S is set to High" in {
      val ((q, nq), state) = buildAndRun { implicit env => norLatch(High, Low) }
      state.get(q) must beSome(true)
      state.get(nq) must beSome(false)
    }

    "be set to Low when R is set to High" in {
      val ((q, nq), state) = buildAndRun { implicit env => norLatch(Low, High) }
      state.get(q) must beSome(false)
      state.get(nq) must beSome(true)
    }

    "retain its original value when both S and R are Low" in {
      val set, reset = new Port
      val ((q, nq), state1) = buildAndRun { implicit env => norLatch(set, reset) }
      state1.get(q) must beNone

      def setAndRun(state: SimState, s: Boolean, r: Boolean): SimState = {
        state.schedule(0, PortChange(set, Some(s)))
        state.schedule(0, PortChange(reset, Some(r)))
        Sim.run(state)
      }

      val state2 = setAndRun(state1, false, false)
      state2.get(q) must beNone
      val state3 = setAndRun(state2, true, false)
      state3.get(q) must beSome(true)
      val state4 = setAndRun(state3, false, false)
      state4.get(q) must beSome(true)
      val state5 = setAndRun(state4, false, true)
      state5.get(q) must beSome(false)
      val state6 = setAndRun(state5, false, false)
      state6.get(q) must beSome(false)
    }
  }
}
