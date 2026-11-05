package component

import component.BuilderAPI.*
import core.*
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import simulator.GateProcessor
import testkit.*
import util.Implicits.*

class SyncSpec extends BaseSpec with SequentialScenarios {

  given Arbitrary[Vector[LogicLevel]] = Arbitrary(
    for {
      n <- Gen.choose(1, 20)
      xs <- Gen.listOfN(n, summon[Arbitrary[LogicLevel]].arbitrary)
    } yield xs.toVector
  )

  def setBus(bus: Bus, vs: Seq[Boolean])(sim: GateProcessor): GateProcessor =
    bus.zip(vs).foldLeft(sim) { case (st, (p, v)) => st.set(p, v) }

  "A clockDomain" should {

    "expose complementary non-overlapping phases" in {
      val clk = newPort()
      val (dom, comp) = buildComponent { clockDomain(clk) }

      val low = GateProcessor.setup(comp).set(clk, false).run()
      (low.get(dom.clkM) must beSome(true)) and
        (low.get(dom.clkS) must beSome(false)) and
        (low.set(clk, true).run().get(dom.clkM) must beSome(false)) and
        (low.set(clk, true).run().get(dom.clkS) must beSome(true))
    }

    "assert resetSync asynchronously and release it synchronously" in {
      val resetN = newPort()
      val (dom, comp) = buildComponent { clockDomain(clock(100), resetN) }

      runPlan(
        comp,
        10 -> { _.set(resetN, false) },
        30 -> { st => st.get(dom.resetSync) must beSome(true) }, // no clock edge needed
        40 -> { _.set(resetN, true) },
        250 -> { st => st.get(dom.resetSync) must beSome(true) }, // one edge is not enough
        450 -> { st => st.get(dom.resetSync) must beSome(false) } // released after two edges
      )
    }
  }

  "A reg" should {

    "start unset" in forAll(Gen.choose(1, 20)) { n =>
      val d = newBus(n)
      val (outs, comp) = buildComponent {
        given ClockDomain = clockDomain(clock(100))
        reg(d)
      }
      val sim = GateProcessor.setupAndRun(comp, Some(1000))
      foreach(outs) { out => sim.get(out) must beNone }
    }

    "capture d on the rising edge when en is High" in forAll { (ins: Vector[LogicLevel]) =>
      val (outs, comp) = buildComponent {
        given ClockDomain = clockDomain(clock(100))
        reg(ins)
      }
      val sim = GateProcessor.setupAndRun(comp, Some(250))
      sim.get(outs).sequence must beSome(ins.map(_.toBool))
    }

    "hold its value when en is Low" in {
      val d = newBus(4)
      val en = newPort()
      val (outs, comp) = buildComponent {
        given ClockDomain = clockDomain(clock(100))
        reg(d, en)
      }
      val v1 = List(true, false, true, false)
      val v2 = List(false, true, false, true)

      runPlan(
        comp,
        25 -> { _.set(en, true) },
        50 -> setBus(d, v1),
        250 -> { st => st.get(outs).sequence must beSome(v1) },
        275 -> { st => setBus(d, v2)(st.set(en, false)) },
        450 -> { st => st.get(outs).sequence must beSome(v1) },
        650 -> { st => st.get(outs).sequence must beSome(v1) }
      )
    }

    "reset synchronously, winning over enable" in {
      val d = newBus(4)
      val en, rst = newPort()
      val (outs, comp) = buildComponent {
        given ClockDomain = clockDomain(clock(100))
        reg(d, en, syncReset = rst, resetValue = 0xa) // 0xA = 1010
      }
      val reset = List(false, true, false, true)

      runPlan(
        comp,
        25 -> { _.set(en, true).set(rst, false) },
        50 -> setBus(d, List(true, true, true, true)),
        250 -> { st => st.get(outs).sequence must beSome(List(true, true, true, true)) },
        275 -> { st => setBus(d, List(false, false, false, false))(st.set(rst, true)) },
        450 -> { st => st.get(outs).sequence must beSome(reset) },
        475 -> { _.set(rst, false) },
        650 -> { st => st.get(outs).sequence must beSome(List(false, false, false, false)) }
      )
    }

    "power-on reset through the domain reset pin" in {
      val d = newBus(4)
      val en, resetN = newPort()
      val (outs, comp) = buildComponent {
        val dom = clockDomain(clock(100), resetN)
        given ClockDomain = dom
        reg(d, en, syncReset = dom.resetSync)
      }

      runPlan(
        comp,
        10 -> { _.set(resetN, false).set(en, true) },
        20 -> setBus(d, List(true, true, true, true)),
        250 -> { st => st.get(outs).sequence must beSome(List(false, false, false, false)) },
        275 -> { _.set(resetN, true) },
        // release takes two edges to propagate; the reg samples the pre-edge resetSync
        450 -> { st => st.get(outs).sequence must beSome(List(false, false, false, false)) },
        650 -> { st => st.get(outs).sequence must beSome(List(false, false, false, false)) },
        850 -> { st => st.get(outs).sequence must beSome(List(true, true, true, true)) }
      )
    }

    "behave well under any combination of the above" in {
      forAll(Gen.choose(1, 10)) { n =>
        val d = newBus(n)
        val en, clk, rst = newPort()
        val resetValue = 0xa
        val resetBits = resetValue.toBoolVec(n)
        val (outs, comp) = buildComponent {
          given ClockDomain = clockDomain(clk)
          reg(d, en, syncReset = rst, resetValue = resetValue)
        }

        var expected = Vector.fill(n)(Option.empty[Boolean])

        SequentialScenario(comp)
          .withPorts(d -> false, en -> false, clk -> true, rst -> false)
          .onStart { _ => expected = Vector.fill(n)(Option.empty[Boolean]) }
          .onPosEdge(clk) { sim =>
            expected = expected.zipWithIndex.map { case (cur, i) =>
              if (sim.isHigh(rst)) Some(resetBits(i))
              else if (sim.isHigh(en)) sim.get(d(i)).orElse(cur)
              else cur
            }
          }
          .check { sim =>
            sim.get(outs) must beEqualTo(expected)
          }
          .run()
      }
    }

    "be empty for an empty input bus" in {
      val (outs, _) = buildAndRun {
        given ClockDomain = clockDomain(Low)
        reg(Vector())
      }
      outs must beEmpty
    }
  }

  "A syncCounter" should {

    "be empty for zero width" in {
      val (outs, _) = buildAndRun {
        given ClockDomain = clockDomain(Low)
        syncCounter(0)
      }
      outs must beEmpty
    }

    "count up on enabled clock edges and hold when disabled" in {
      val en, load = newPort()
      val (outs, comp) = buildComponent {
        given ClockDomain = clockDomain(clock(100))
        syncCounter(2, en, load)
      }
      outs must haveLength(2)

      runPlan(
        comp,
        25 -> { _.set(en, true).set(load, true) }, // load 0: the counter powers up unset
        225 -> { _.set(load, false) },
        250 -> { st => st.get(outs).sequence.map(_.toInt) must beSome(0) },
        450 -> { st => st.get(outs).sequence.map(_.toInt) must beSome(1) },
        650 -> { st => st.get(outs).sequence.map(_.toInt) must beSome(2) },
        850 -> { st => st.get(outs).sequence.map(_.toInt) must beSome(3) },
        1050 -> { st => st.get(outs).sequence.map(_.toInt) must beSome(0) },
        1075 -> { _.set(en, false) },
        1250 -> { st => st.get(outs).sequence.map(_.toInt) must beSome(0) }
      )
    }

    "load synchronously, winning over enable" in {
      val en, load = newPort()
      val (outs, comp) = buildComponent {
        given ClockDomain = clockDomain(clock(100))
        syncCounter(4, en, load, loadValue = 0xa)
      }

      runPlan(
        comp,
        25 -> { _.set(en, true).set(load, true) },
        250 -> { st => st.get(outs).sequence.map(_.toInt) must beSome(0xa) },
        275 -> { _.set(load, false) },
        450 -> { st => st.get(outs).sequence.map(_.toInt) must beSome(0xb) },
        650 -> { st => st.get(outs).sequence.map(_.toInt) must beSome(0xc) }
      )
    }

    "behave as a synchronous binary counter" in {
      forAll(Gen.choose(1, 10)) { n =>
        val en, load, clk = newPort()
        val loadValue = 5
        val (outs, comp) = buildComponent {
          given ClockDomain = clockDomain(clk)
          syncCounter(n, en, load, loadValue = loadValue)
        }

        var expected = 0
        var started = false

        SequentialScenario(comp)
          .withPorts(en -> false, load -> false, clk -> true)
          .onStart { _ => { expected = 0; started = false } }
          .beforeAction {
            // the counter powers up unset: force a load before the first clock edge
            // so the model and the hardware agree from a known state; the `.run()`
            // settles it before the edge (setup time)
            case (sim, _, _, _) if !started => sim.set(load, true).run()
            case (sim, _, _, _) => sim
          }
          .onPosEdge(clk) { sim =>
            if (sim.isHigh(load)) {
              started = true
              expected = loadValue % (1 << n)
            } else if (started && sim.isHigh(en)) {
              expected = (expected + 1) % (1 << n)
            }
          }
          .check { sim =>
            if (started) sim.get(outs).sequence.map(_.toInt) must beSome(expected)
            else foreach(outs) { o => sim.get(o) must beNone }
          }
          .run()
      }
    }
  }

  "A shiftReg" should {

    "be empty for zero width" in {
      val (outs, _) = buildAndRun {
        given ClockDomain = clockDomain(Low)
        shiftReg(0, Low)
      }
      outs must beEmpty
    }

    "shift toward the MSB by default" in {
      val in, en = newPort()
      val (outs, comp) = buildComponent {
        given ClockDomain = clockDomain(clock(100))
        shiftReg(4, in, en)
      }

      // unset bits pipeline through one stage per clock
      runPlan(
        comp,
        25 -> { _.set(en, true).set(in, true) },
        250 -> { st => st.get(outs) must beEqualTo(Vector(Some(true), None, None, None)) },
        450 -> { st => st.get(outs) must beEqualTo(Vector(Some(true), Some(true), None, None)) },
        650 -> { st => st.get(outs) must beEqualTo(Vector(Some(true), Some(true), Some(true), None)) },
        850 -> { st => st.get(outs) must beEqualTo(Vector(Some(true), Some(true), Some(true), Some(true))) },
        875 -> { _.set(in, false) },
        1050 -> { st => st.get(outs) must beEqualTo(Vector(Some(false), Some(true), Some(true), Some(true))) },
        1250 -> { st => st.get(outs) must beEqualTo(Vector(Some(false), Some(false), Some(true), Some(true))) }
      )
    }

    "shift toward the LSB when dir is High" in {
      val in, en = newPort()
      val (outs, comp) = buildComponent {
        given ClockDomain = clockDomain(clock(100))
        shiftReg(4, in, en, dir = High)
      }

      runPlan(
        comp,
        25 -> { _.set(en, true).set(in, true) },
        250 -> { st => st.get(outs) must beEqualTo(Vector(None, None, None, Some(true))) },
        450 -> { st => st.get(outs) must beEqualTo(Vector(None, None, Some(true), Some(true))) },
        650 -> { st => st.get(outs) must beEqualTo(Vector(None, Some(true), Some(true), Some(true))) },
        850 -> { st => st.get(outs) must beEqualTo(Vector(Some(true), Some(true), Some(true), Some(true))) },
        875 -> { _.set(in, false) },
        1050 -> { st => st.get(outs) must beEqualTo(Vector(Some(true), Some(true), Some(true), Some(false))) },
        1250 -> { st => st.get(outs) must beEqualTo(Vector(Some(true), Some(true), Some(false), Some(false))) }
      )
    }

    "hold its contents when en is Low" in {
      val in, en = newPort()
      val (outs, comp) = buildComponent {
        given ClockDomain = clockDomain(clock(100))
        shiftReg(4, in, en)
      }

      runPlan(
        comp,
        25 -> { _.set(en, true).set(in, true) },
        250 -> { st => st.get(outs) must beEqualTo(Vector(Some(true), None, None, None)) },
        275 -> { _.set(en, false) },
        1050 -> { st => st.get(outs) must beEqualTo(Vector(Some(true), None, None, None)) }
      )
    }

    "behave as a shift register in both directions" in {
      forAll(Gen.choose(1, 10)) { n =>
        val in, en, dir, clk = newPort()
        val (outs, comp) = buildComponent {
          given ClockDomain = clockDomain(clk)
          shiftReg(n, in, en, dir)
        }

        var expected = Vector.fill(n)(Option.empty[Boolean])

        SequentialScenario(comp)
          .withPorts(in -> false, en -> false, dir -> false, clk -> true)
          .onStart { _ => expected = Vector.fill(n)(Option.empty[Boolean]) }
          .onPosEdge(clk) { sim =>
            if (sim.isHigh(en)) {
              val v = sim.get(in)
              expected =
                if (sim.isHigh(dir)) expected.tail :+ v
                else v +: expected.init
            }
          }
          .check { sim =>
            sim.get(outs) must beEqualTo(expected)
          }
          .run()
      }
    }
  }
}
