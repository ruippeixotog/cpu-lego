package component

import component.BuilderAPI.*
import core.*
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import simulator.Sim
import testkit.*
import util.Implicits.*

class MemorySpec extends BaseSpec with SequentialScenarios {

  given Arbitrary[Vector[LogicLevel]] = Arbitrary(
    for {
      n <- Gen.choose(1, 20)
      xs <- Gen.listOfN(n, summon[Arbitrary[LogicLevel]].arbitrary)
    } yield xs.toVector
  )

  "A latchClocked" should {

    "start unset" in {
      val ((q, nq), comp) = buildComponent { latchClocked(new Port, new Port, clock(100)) }
      val sim = Sim.setupAndRun(comp, Some(1000))
      sim.get(q) must beNone
      sim.get(nq) must beNone
    }

    "remain unset while clk is Low" in {
      val ((q, nq), sim) = buildAndRun { latchClocked(High, Low, Low) }
      sim.get(q) must beNone
      sim.get(nq) must beNone
    }

    "be set to High when S is set to High and clk is High" in {
      val ((q, nq), sim) = buildAndRun { latchClocked(High, Low, High) }
      sim.get(q) must beSome(true)
      sim.get(nq) must beSome(false)
    }

    "be set unconditionally to High when preset is Low" in {
      val ((q, nq), sim) = buildAndRun { latchClocked(Low, High, Low, preset = Low) }
      sim.get(q) must beSome(true)
      sim.get(nq) must beSome(false)
    }

    "be set to Low when R is set to High and clk is High" in {
      val ((q, nq), sim) = buildAndRun { latchClocked(Low, High, High) }
      sim.get(q) must beSome(false)
      sim.get(nq) must beSome(true)
    }

    "be set unconditionally to Low when clear is Low" in {
      val ((q, nq), sim) = buildAndRun { latchClocked(High, Low, Low, clear = Low) }
      sim.get(q) must beSome(false)
      sim.get(nq) must beSome(true)
    }

    "retain its original value when both S and R are Low" in {
      val set, reset = new Port
      val ((q, nq), comp) = buildComponent { latchClocked(set, reset, clock(100)) }

      def setInputs(s: Boolean, r: Boolean)(sim: Sim) =
        sim.set(set, s).set(reset, r)

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
        .withPorts(set, reset, clock -> true)
        .onStart { _ => expectedQ = None }
        .beforeAction {
          // ensure `set` and `reset` are not High at the same time
          case (sim, `set`, true, _) => sim.set(reset, false)
          case (sim, `reset`, true, _) => sim.set(set, false)
          case (sim, _, _, _) => sim
        }
        .whenHigh(clock) { sim =>
          expectedQ = (sim.get(set), sim.get(reset)) match {
            case (Some(true), _) => Some(true)
            case (_, Some(true)) => Some(false)
            case _ => expectedQ
          }
        }
        .check { sim =>
          sim.get(q) must beEqualTo(expectedQ)
          sim.get(nq) must beEqualTo(expectedQ.map(!_))
        }
        .run()
    }
  }

  "A dLatch" should {

    "start unset" in {
      val ((q, nq), comp) = buildComponent { dLatch(new Port, clock(100)) }
      val sim = Sim.setupAndRun(comp, Some(1000))
      sim.get(q) must beNone
      sim.get(nq) must beNone
    }

    "be set to the input on positive edge trigger" in forAll { (in: LogicLevel) =>
      val ((q, nq), comp) = buildComponent { dLatch(in, clock(100)) }
      val sim = Sim.setupAndRun(comp, Some(250))
      sim.get(q) must beSome(in.toBool)
      sim.get(nq) must beSome(!in.toBool)
    }

    "retain its original value outside positive edges" in {
      val in = new Port
      val ((q, nq), comp) = buildComponent { dLatch(in, clock(100)) }

      runPlan(
        comp,
        25 -> { _.get(q) must beNone },
        50 -> { _.set(in, true) },
        75 -> { _.get(q) must beNone },
        150 -> { _.get(q) must beNone },
        250 -> { _.get(q) must beSome(true) },
        300 -> { _.set(in, false) },
        350 -> { _.get(q) must beSome(true) },
        450 -> { _.get(q) must beSome(false) }
      )
    }

    "behave well under any combination of the above" in {
      val d, clock = newPort()
      val ((q, nq), comp) = buildComponent { dLatch(d, clock) }

      var expectedQ = Option.empty[Boolean]

      SequentialScenario(comp)
        .withPorts(d, clock -> true)
        .onStart { _ => expectedQ = None }
        .onPosEdge(clock) { sim =>
          expectedQ = sim.get(d).orElse(expectedQ)
        }
        .check { sim =>
          sim.get(q) must beEqualTo(expectedQ)
          sim.get(nq) must beEqualTo(expectedQ.map(!_))
        }
        .run()
    }
  }

  "A jkFlipFlop" should {

    "start unset" in {
      val ((q, nq), comp) = buildComponent { jkFlipFlop(new Port, new Port, clock(100), High) }
      val sim = Sim.setupAndRun(comp, Some(1000))
      sim.get(q) must beNone
      sim.get(nq) must beNone
    }

    "toggle the output when both inputs are High" in {
      val j, k, clear = newPort()
      val ((q, nq), comp) = buildComponent { jkFlipFlop(j, k, clock(100), clear) }

      def setInput(set: Boolean, reset: Boolean)(sim: Sim) =
        sim.set(j, set).set(k, reset)

      runPlan(
        comp,
        10 -> { _.set(clear, false) },
        20 -> { _.set(clear, true) },
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

    "behave as a JK flip-flop" in {
      val j, k, clk, clear = newPort()
      val ((q, nq), comp) = buildComponent { jkFlipFlop(j, k, clk, clear) }

      var expectedQ = Option.empty[Boolean]

      SequentialScenario(comp)
        .withPorts(j -> false, k -> false, clk -> true, clear -> false)
        .onStart { _ => expectedQ = Some(false) }
        .onPosEdge(clk) { sim =>
          expectedQ = (sim.get(j), sim.get(k)) match {
            case (Some(true), Some(false)) => Some(true)
            case (Some(false), Some(true)) => Some(false)
            case (Some(true), Some(true)) => expectedQ.map(!_)
            case _ => expectedQ
          }
        }
        .whenLow(clear) { _ => expectedQ = Some(false) }
        .check { sim =>
          sim.get(q) must beEqualTo(expectedQ)
          sim.get(nq) must beEqualTo(expectedQ.map(!_))
        }
        .run()
    }
  }

  "A register" should {

    "start unset" in forAll(Gen.choose(1, 20)) { n =>
      val ins = newBus(n)
      val (outs, comp) = buildComponent { register(ins, Low, clock(100)) }
      val sim = Sim.setupAndRun(comp, Some(1000))
      foreach(outs) { out => sim.get(out) must beNone }
    }

    "remain unset while clk is Low" in forAll { (ins: Vector[LogicLevel]) =>
      val (outs, sim) = buildAndRun { register(ins, High, Low) }
      foreach(outs) { out => sim.get(out) must beNone }
    }

    "remain unset while load is Low" in forAll { (ins: Vector[LogicLevel]) =>
      val (outs, comp) = buildComponent { register(ins, Low, clock(100)) }
      val sim = Sim.setupAndRun(comp, Some(1000))
      foreach(outs) { out => sim.get(out) must beNone }
    }

    "be set to the input on clock positive edge when load is High" in forAll { (ins: Vector[LogicLevel]) =>
      val (outs, comp) = buildComponent { register(ins, High, clock(100)) }
      val sim = Sim.setupAndRun(comp, Some(250))
      sim.get(outs).sequence must beSome.which { bools =>
        bools must beEqualTo(ins.map(_.toBool))
      }
    }

    "be set unconditionally to Low when clear is Low" in forAll { (ins: Vector[LogicLevel]) =>
      val (outs, sim) = buildAndRun { register(ins, Low, Low, clear = Low) }
      sim.get(outs).sequence must beSome(List.fill(ins.length)(false))
    }

    "retain its original value outside positive edges or when load is Low" in {
      val ins = newBus(4)
      val load = newPort()
      val (outs, comp) = buildComponent { register(ins, load, clock(100)) }

      def setInputs(load0: Boolean, ins0: List[Boolean])(sim: Sim) = {
        val st0 = sim.set(load, load0)
        ins.zip(ins0).foldLeft(st0) { case (st, (port, v)) => st.set(port, v) }
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
        val xs = newBus(n)
        val load, clk, clear = newPort()
        val (outs, comp) = buildComponent { register(xs, load, clk, clear) }

        var expectedOuts = Vector.fill(n)(Option.empty[Boolean])

        SequentialScenario(comp)
          .withPorts(load -> false, clk -> true, clear -> false, xs)
          .onStart { _ => expectedOuts = Vector.fill(n)(Some(false)) }
          .onPosEdge(clk) { sim =>
            if (sim.isHigh(load)) {
              expectedOuts = xs.zip(expectedOuts).map { case (x, curr) =>
                sim.get(x).orElse(curr)
              }
            }
          }
          .whenLow(clear) { _ =>
            expectedOuts = Vector.fill(n)(Some(false))
          }
          .check { sim =>
            sim.get(outs) must beEqualTo(expectedOuts)
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

      def getOutAsInt(sim: Sim): Option[Int] =
        sim.get(outs).sequence.map(_.toInt)

      runPlan(
        comp,
        0 -> { _.set(clear, false) },
        25 -> { _.set(clear, true) },
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
          .withPorts(count -> false, clk -> true, clear -> false)
          .onStart { _ => expectedOut = 0 }
          .onNegEdge(clk) { sim =>
            if (sim.isHigh(count)) {
              expectedOut = (expectedOut + 1) % (1 << n)
            }
          }
          .whenLow(clear) { _ => expectedOut = 0 }
          .check { sim =>
            sim.get(outs).sequence.map(_.toInt) must beSome(expectedOut)
          }
          .run()
      }
    }
  }

  "A presettableCounter" should {

    "behave as a pressetable binary counter" in {
      forAll(Gen.choose(1, 10)) { n =>
        val ps = newBus(n)
        val load, clk, clear = newPort()
        val (outs, comp) = buildComponent { presettableCounter(ps, load, clk, clear) }
        outs must haveLength(n)

        var expectedOut = 0

        SequentialScenario(comp)
          .withPorts(load -> false, clk -> true, clear -> false, ps -> false)
          .onStart { _ => expectedOut = 0 }
          .onNegEdge(clk) { sim =>
            expectedOut = (expectedOut + 1) % (1 << n)
          }
          .whenHigh(load) { sim => expectedOut = sim.get(ps).sequence.get.toInt }
          .whenLow(clear) { _ => expectedOut = 0 }
          .check { sim =>
            sim.get(outs).sequence.map(_.toInt) must beSome(expectedOut)
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
          .withPorts(clk -> true, clear -> false)
          .onStart { _ => expectedIdx = 0 }
          .onNegEdge(clk) { _ => expectedIdx = (expectedIdx + 1) % n }
          .whenLow(clear) { _ => expectedIdx = 0 }
          .check { sim =>
            foreach(outs.zipWithIndex) { case (out, idx) =>
              sim.get(out) must beSome(idx == expectedIdx)
            }
          }
          .run()
      }
    }
  }

  "A RAM" should {

    "behave as a read-write random access memory" in {
      forAll(Gen.choose(1, 4), Gen.choose(1, 6)) { (inN, addrN) =>
        val ins = newBus(inN)
        val addr = newBus(addrN)
        val we, ce = newPort()
        val (outs, comp) = buildComponent { ram(ins, addr, we, ce) }
        outs must haveLength(inN)

        var mem: Array[Vector[Option[Boolean]]] = Array.fill(1 << addrN)(Vector.fill(inN)(None))

        def addrIdx(sim: Sim) = sim.get(addr).sequence.get.toInt

        SequentialScenario(comp)
          .withPorts(we -> false, ce -> false, ins -> false, addr -> false)
          .onStart { _ => mem = Array.fill(1 << addrN)(Vector.fill(inN)(None)) }
          .whenHigh(we) { sim => mem(addrIdx(sim)) = sim.get(ins) }
          .check { sim =>
            sim.get(outs) must beEqualTo(
              if (sim.isHigh(ce)) mem(addrIdx(sim))
              else Vector.fill(inN)(None)
            )
          }
          .run()
      }
    }
  }

  "A ROM" should {

    "behave as a read-only memory" in {

      forAll(Gen.choose(1, 6)) { addrN =>
        forAll(Gen.listOfN(1 << addrN, Gen.listOfN(4, Gen.oneOf(false, true)))) { data =>
          val addr = newBus(addrN)
          val (outs, comp) = buildComponent { rom(data, addr) }
          outs must haveLength(data.head.length)

          def addrIdx(sim: Sim) = sim.get(addr).sequence.get.toInt

          SequentialScenario(comp)
            .withPorts(addr -> false)
            .check { sim =>
              sim.get(outs) must beEqualTo(data(addrIdx(sim)).map(Some.apply))
            }
            .run()
        }
      }
    }
  }
}
