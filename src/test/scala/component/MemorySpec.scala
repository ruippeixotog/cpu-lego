package component

import component.BuilderAPI._
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
      val ((q, nq), comp) = buildComponent { latchClocked(new Port, new Port, clock(100)) }
      val state = Sim.runComponent(comp, Some(1000))
      state.get(q) must beNone
      state.get(nq) must beNone
    }

    "remain unset while clk is Low" in {
      val ((q, nq), state) = buildAndRun { latchClocked(High, Low, Low) }
      state.get(q) must beNone
      state.get(nq) must beNone
    }

    "be set to High when S is set to High and clk is High" in {
      val ((q, nq), state) = buildAndRun { latchClocked(High, Low, High) }
      state.get(q) must beSome(true)
      state.get(nq) must beSome(false)
    }

    "be set unconditionally to High when preset is Low" in {
      val ((q, nq), state) = buildAndRun { latchClocked(Low, High, Low, preset = Low) }
      state.get(q) must beSome(true)
      state.get(nq) must beSome(false)
    }

    "be set to Low when R is set to High and clk is High" in {
      val ((q, nq), state) = buildAndRun { latchClocked(Low, High, High) }
      state.get(q) must beSome(false)
      state.get(nq) must beSome(true)
    }

    "be set unconditionally to Low when clear is Low" in {
      val ((q, nq), state) = buildAndRun { latchClocked(High, Low, Low, clear = Low) }
      state.get(q) must beSome(false)
      state.get(nq) must beSome(true)
    }

    "retain its original value when both S and R are Low" in {
      val set, reset = new Port
      val ((q, nq), comp) = buildComponent { latchClocked(set, reset, clock(100)) }

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
      val ((q, nq), comp) = buildComponent { latchClocked(set, reset, clock) }

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
          if (state.get(clock) == Some(true)) {
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
      val ((q, nq), comp) = buildComponent { dLatch(new Port, clock(100)) }
      val state = Sim.runComponent(comp, Some(1000))
      state.get(q) must beNone
      state.get(nq) must beNone
    }

    "be set to the input on positive edge trigger" in forAll { (in: LogicLevel) =>
      val ((q, nq), comp) = buildComponent { dLatch(in, clock(100)) }
      val state = Sim.runComponent(comp, Some(250))
      state.get(q) must beSome(in.toBool)
      state.get(nq) must beSome(!in.toBool)
    }

    "retain its original value outside positive edges" in {
      val in = new Port
      val ((q, nq), comp) = buildComponent { dLatch(in, clock(100)) }

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
      val ((q, nq), comp) = buildComponent { dLatch(d, clock) }

      var expectedQ = Option.empty[Boolean]

      SequentialScenario(comp)
        .withPorts(d -> None, clock -> Some(true))
        .onStart { _ => expectedQ = None }
        .onPosEdge(clock) { state =>
          expectedQ = state.get(d).orElse(expectedQ)
        }
        .check { state =>
          state.get(q) must beEqualTo(expectedQ)
          state.get(nq) must beEqualTo(expectedQ.map(!_))
        }
        .run()
    }
  }

  "A jkMasterSlave" should {

    "start unset" in {
      val ((q, nq), comp) = buildComponent { jkMasterSlave(new Port, new Port, clock(100), High) }
      val state = Sim.runComponent(comp, Some(1000))
      state.get(q) must beNone
      state.get(nq) must beNone
    }

    "toggle the output when both inputs are High" in {
      val j, k, clear = newPort()
      val ((q, nq), comp) = buildComponent { jkMasterSlave(j, k, clock(100), clear) }

      def setClear(clr: Boolean)(state: SimState) =
        state.schedule(0, PortChange(clear, Some(clr)))

      def setInput(set: Boolean, reset: Boolean)(state: SimState) = {
        state.schedule(0, PortChange(j, Some(set)))
        state.schedule(0, PortChange(k, Some(reset)))
      }

      runPlan(
        comp,
        10 -> setClear(false),
        20 -> setClear(true),
        50 -> { _.get(q) must beSome(false) },
        50 -> setInput(true, false),
        75 -> { _.get(q) must beSome(false) },
        150 -> { _.get(q) must beSome(false) },
        250 -> { _.get(q) must beSome(true) },
        300 -> setInput(false, true),
        350 -> { _.get(q) must beSome(true) },
        450 -> { _.get(q) must beSome(false) },
        500 -> setInput(true, true),
        650 -> { _.get(q) must beSome(true) },
        850 -> { _.get(q) must beSome(false) }
      )
    }

    "behave as a JK master-slave flip-flop" in {
      val j, k, clk, clear = newPort()
      val ((q, nq), comp) = buildComponent { jkMasterSlave(j, k, clk, clear) }

      var expectedQ = Option.empty[Boolean]

      SequentialScenario(comp)
        .withPorts(j -> Some(false), k -> Some(false), clk -> Some(true), clear -> Some(false))
        .onStart { _ => expectedQ = Some(false) }
        .onPosEdge(clk) { state =>
          expectedQ = (state.get(j), state.get(k)) match {
            case (Some(true), Some(false)) => Some(true)
            case (Some(false), Some(true)) => Some(false)
            case (Some(true), Some(true)) => expectedQ.map(!_)
            case _ => expectedQ
          }
        }
        .onAction { (state, _, _, _) =>
          if (state.get(clear) == Some(false)) {
            expectedQ = Some(false)
          }
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
      val (outs, comp) = buildComponent { register(Array.fill(n)(new Port), Low, clock(100)) }
      val state = Sim.runComponent(comp, Some(1000))
      foreach(outs) { out => state.get(out) must beNone }
    }

    "remain unset while clk is Low" in forAll { (ins: List[LogicLevel]) =>
      val (outs, state) = buildAndRun { register(ins, High, Low) }
      foreach(outs) { out => state.get(out) must beNone }
    }

    "remain unset while load is Low" in forAll { (ins: List[LogicLevel]) =>
      val (outs, comp) = buildComponent { register(ins, Low, clock(100)) }
      val state = Sim.runComponent(comp, Some(1000))
      foreach(outs) { out => state.get(out) must beNone }
    }

    "be set to the input on clock positive edge when load is High" in forAll { (ins: List[LogicLevel]) =>
      val (outs, comp) = buildComponent { register(ins, High, clock(100)) }
      val state = Sim.runComponent(comp, Some(250))
      outs.map(state.get).sequence must beSome.which { bools =>
        bools must beEqualTo(ins.map(_.toBool))
      }
    }

    "be set unconditionally to Low when clear is Low" in forAll { (ins: List[LogicLevel]) =>
      val (outs, state) = buildAndRun { register(ins, Low, Low, clear = Low) }
      outs.map(state.get).sequence must beSome(List.fill(ins.length)(false))
    }

    "retain its original value outside positive edges or when load is Low" in {
      val load = new Port
      val ins = Array.fill(4)(new Port)
      val (outs, comp) = buildComponent { register(ins, load, clock(100)) }

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
        val (outs, comp) = buildComponent { register(xs, load, clk, clear) }

        val inits = load -> Some(false) :: clk -> Some(true) :: clear -> Some(false) :: xs.map(_ -> None)
        var expectedOuts = List.fill(n)(Option.empty[Boolean])

        SequentialScenario(comp)
          .withPorts(inits: _*)
          .onStart { _ => expectedOuts = List.fill(n)(Some(false)) }
          .onPosEdge(clk) { state =>
            if (state.get(load) == Some(true)) {
              expectedOuts = xs.zip(expectedOuts).map { case (x, curr) =>
                state.get(x).orElse(curr)
              }
            }
          }
          .onAction { (state, _, _, _) =>
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

  "A counter" should {

    "count the number of clock cycles when count is High" in {
      val clear = newPort()
      val (outs, comp) = buildComponent { counter(2, High, clock(100), clear) }
      outs must haveLength(2)

      def setClear(clr: Boolean)(state: SimState) =
        state.schedule(0, PortChange(clear, Some(clr)))

      def getOutAsInt(state: SimState): Option[Int] =
        outs.map(state.get).sequence.map(_.toInt)

      runPlan(
        comp,
        0 -> setClear(false),
        25 -> setClear(true),
        50 -> { st => getOutAsInt(st) must beSome(0) },
        125 -> { st => getOutAsInt(st) must beSome(1) },
        250 -> { st => getOutAsInt(st) must beSome(1) },
        350 -> { st => getOutAsInt(st) must beSome(2) },
        550 -> { st => getOutAsInt(st) must beSome(3) },
        750 -> { st => getOutAsInt(st) must beSome(0) },
        950 -> { st => getOutAsInt(st) must beSome(1) }
      )
    }

    "behave as a controlled binary counter" in {
      forAll(Gen.choose(1, 10)) { n =>
        val count, clk, clear = newPort()
        val (outs, comp) = buildComponent { counter(n, count, clk, clear) }
        outs must haveLength(n)

        var expectedOut = 0

        SequentialScenario(comp)
          .withPorts(count -> Some(false), clk -> Some(true), clear -> Some(false))
          .onStart { _ => expectedOut = 0 }
          .onNegEdge(clk) { state =>
            if (state.get(count) == Some(true)) {
              expectedOut = (expectedOut + 1) % (1 << n)
            }
          }
          .onAction { (state, _, _, _) =>
            if (state.get(clear) == Some(false)) {
              expectedOut = 0
            }
          }
          .check { state =>
            outs.map(state.get).sequence.map(_.toInt) must beSome(expectedOut)
          }
          .run()
      }
    }
  }

  "A ring counter" should {

    "behave as a ring counter" in {
      forAll(Gen.choose(1, 10)) { n =>
        val clk, clear = newPort()
        val (outs, comp) = buildComponent { ringCounter(n, clk, clear) }
        outs must haveLength(n)

        var expectedIdx = 0

        SequentialScenario(comp)
          .withPorts(clk -> Some(true), clear -> Some(false))
          .onStart { _ => expectedIdx = 0 }
          .onPosEdge(clk) { _ => expectedIdx = (expectedIdx + 1) % n }
          .onAction { (state, _, _, _) =>
            if (state.get(clear) == Some(false)) {
              expectedIdx = 0
            }
          }
          .check { state =>
            foreach(outs.zipWithIndex) { case (out, idx) =>
              state.get(out) must beSome(idx == expectedIdx)
            }
          }
          .run()
      }
    }
  }
}
