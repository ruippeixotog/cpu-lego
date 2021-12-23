package component

import component.BuilderDSL._
import core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import simulator.{PortChange, Sim, SimState}
import testkit._

class MemorySpec extends BaseSpec with SequentialScenarios {

  given Arbitrary[List[LogicLevel]] = Arbitrary(
    Gen.choose(1, 20).flatMap { n => Gen.listOfN(n, summon[Arbitrary[LogicLevel]].arbitrary) }
  )

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

    "behave well under any combination of the above" in {
      val set, reset, clock = newPort()
      val ((q, nq), comp) = buildComponent { implicit env => latchClocked(set, reset, clock) }

      var expectedQ = Option.empty[Boolean]

      SequentialScenario(comp)
        .withPorts(set -> None, reset -> None, clock -> Some(true))
        .onStart { _ => expectedQ = None }
        .beforeAction {
          // ensure `set` and `reset` are not High at the same time
          case (state, `set`, true, _) => state.schedule(0, PortChange(reset, Some(false)))
          case (state, `reset`, true, _) => state.schedule(0, PortChange(set, Some(false)))
          case _ => // do nothing
        }
        .onAction { (state, _, _, _) =>
          if (state.get(clock).contains(true)) {
            expectedQ = (state.get(set), state.get(reset)) match {
              case (Some(true), _) => Some(true)
              case (_, Some(true)) => Some(false)
              case _ => expectedQ
            }
          }
        }
        .check { state =>
          state.get(q) must beEqualTo(expectedQ)
          state.get(nq) must beEqualTo(expectedQ.map(!_))
        }
        .run()
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
      val state = Sim.runComponent(comp, Some(250))
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

    "behave well under any combination of the above" in {
      val d, clock = newPort()
      val ((q, nq), comp) = buildComponent { implicit env => dLatch(d, clock) }

      var expectedQ = Option.empty[Boolean]

      SequentialScenario(comp)
        .withPorts(d -> None, clock -> Some(true))
        .onStart { _ => expectedQ = None }
        .onAction {
          case (state, `clock`, true, Some(false)) =>
            expectedQ = state.get(d).orElse(expectedQ)
          case _ => // do nothing
        }
        .check { state =>
          state.get(q) must beEqualTo(expectedQ)
          state.get(nq) must beEqualTo(expectedQ.map(!_))
        }
        .run()
    }
  }

  "A register" should {

    "start unset" in forAll(Gen.choose(1, 20)) { n =>
      val (outs, comp) = buildComponent { implicit env =>
        register(Array.fill(n)(new Port), Low, clock(100))
      }
      val state = Sim.runComponent(comp, Some(1000))
      foreach(outs) { out => state.get(out) must beNone }
    }

    "remain unset while clk is Low" in forAll { (ins: List[LogicLevel]) =>
      val (outs, state) = buildAndRun { implicit env => register(ins, High, Low) }
      foreach(outs) { out => state.get(out) must beNone }
    }

    "remain unset while load is Low" in forAll { (ins: List[LogicLevel]) =>
      val (outs, comp) = buildComponent { implicit env => register(ins, Low, clock(100)) }
      val state = Sim.runComponent(comp, Some(1000))
      foreach(outs) { out => state.get(out) must beNone }
    }

    "be set to the input on clock positive edge when load is High" in forAll { (ins: List[LogicLevel]) =>
      val (outs, comp) = buildComponent { implicit env => register(ins, High, clock(100)) }
      val state = Sim.runComponent(comp, Some(250))
      outs.map(state.get).sequence must beSome.which { bools =>
        bools must beEqualTo(ins.map(_.toBool))
      }
    }

    "be set unconditionally to Low when clear is Low" in forAll { (ins: List[LogicLevel]) =>
      val (outs, state) = buildAndRun { implicit env => register(ins, Low, Low, clear = Low) }
      outs.map(state.get).sequence must beSome(List.fill(ins.length)(false))
    }

    "retain its original value outside positive edges or when load is Low" in {
      val load = new Port
      val ins = Array.fill(4)(new Port)
      val (outs, comp) = buildComponent { implicit env => register(ins, load, clock(100)) }

      def setInputs(load0: Boolean, ins0: List[Boolean])(state: SimState) = {
        state.schedule(0, PortChange(load, Some(load0)))
        ins.zip(ins0).foreach { case (port, v) => state.schedule(0, PortChange(port, Some(v))) }
      }

      val val1 = List(true, false, false, true)
      val val2 = List(true, false, true, false)

      runPlan(
        comp,
        25 -> { st => foreach(outs) { st.get(_) must beNone } },
        50 -> setInputs(false, val1),
        250 -> { st => foreach(outs) { st.get(_) must beNone } },
        275 -> setInputs(true, val1),
        300 -> { st => foreach(outs) { st.get(_) must beNone } },
        350 -> { st => foreach(outs) { st.get(_) must beNone } },
        450 -> { st => outs.map(st.get).sequence must beSome(val1) },
        475 -> setInputs(false, val2),
        650 -> { st => outs.map(st.get).sequence must beSome(val1) },
        675 -> setInputs(true, val2),
        750 -> { st => outs.map(st.get).sequence must beSome(val1) },
        850 -> { st => outs.map(st.get).sequence must beSome(val2) }
      )
    }

    "behave well under any combination of the above" in {
      forAll(Gen.choose(1, 10)) { n =>
        val xs = List.fill(n)(newPort())
        val load, clk, clear = newPort()
        val (outs, comp) = buildComponent { implicit env => register(xs, load, clk, clear) }

        val inits = load -> Some(false) :: clk -> Some(true) :: clear -> Some(false) :: xs.map(_ -> None)
        var expectedOuts = List.fill(n)(Option.empty[Boolean])

        SequentialScenario(comp)
          .withPorts(inits: _*)
          .onStart { _ => expectedOuts = List.fill(n)(Some(false)) }
          .onAction { (state, port, newVal, oldVal) =>
            if (port == clk && newVal && oldVal == Some(false) && state.get(load) == Some(true)) {
              expectedOuts = xs.zip(expectedOuts).map { case (x, curr) =>
                state.get(x).orElse(curr)
              }
            }
            if (state.get(clear) == Some(false)) {
              expectedOuts = List.fill(n)(Some(false))
            }
          }
          .check { state =>
            outs.map(state.get) must beEqualTo(expectedOuts)
          }
          .run()
      }
    }
  }
}
