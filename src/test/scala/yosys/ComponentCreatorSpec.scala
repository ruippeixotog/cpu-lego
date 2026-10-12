package yosys

import java.nio.file.{Path, Paths}

import component.BuilderAPI.*
import core.*
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import simulator.GateProcessor
import simulator.Index
import testkit.*
import util.Implicits.*

class ComponentCreatorSpec extends BaseSpec with SequentialScenarios {
  def resourcePath(res: String) = Path.of(getClass.getResource("/" + res).toURI)

  val aoiCreatorFromJson = ComponentCreator.fromYosysJsonFile(resourcePath("aoi.json"))
  val latchCreatorFromJson = ComponentCreator.fromYosysJsonFile(resourcePath("dlatch.json"))
  val aoiCreatorFromHdl = ComponentCreator.fromVerilog(resourcePath("aoi.v"))
  val latchCreatorFromHdl = ComponentCreator.fromVerilog(resourcePath("dlatch.v"))
  val tristateCreatorFromHdl = ComponentCreator.fromVerilog(resourcePath("tristate.v"))
  val hierCreatorFromHdl = ComponentCreator.fromVerilog(resourcePath("hier.v"))

  def aoi(cc: ComponentCreator, a: Port, b: Port, c: Port, d: Port): Spec[Port] = {
    val outs = cc.create("aoi", Map("A" -> a, "B" -> b, "C" -> c, "D" -> d))
    outs("F").asInstanceOf[Port]
  }

  def dLatch(cc: ComponentCreator, d: Port, en: Port, rstn: Port): Spec[Port] = {
    val outs = cc.create("d_latch", Map("d" -> d, "en" -> en, "rstn" -> rstn))
    outs("q").asInstanceOf[Port]
  }

  def tristateY(
      cc: ComponentCreator,
      a: Port,
      b: Port,
      selA: Port,
      selB: Port,
      bus: Bus
  ): Spec[Port] = {
    val outs = cc.create(
      "tristate",
      Map("a" -> a, "b" -> b, "sel_a" -> selA, "sel_b" -> selB, "bus" -> bus)
    )
    outs("y").asInstanceOf[Port]
  }

  List(
    "A JSON-generated AOI" -> aoiCreatorFromJson,
    "A Verilog-generated AOI" -> aoiCreatorFromHdl
  ).foreach { case (desc, cc) =>
    s"$desc" should {
      "compute ~((a & b) | (c & d))" in forAll { (a: LogicLevel, b: LogicLevel, c: LogicLevel, d: LogicLevel) =>
        val expected = !((a.toBool && b.toBool) || (c.toBool && d.toBool))
        val (f, sim) = buildAndRun { aoi(cc, a, b, c, d) }
        sim.get(f) must beSome(expected)
      }
    }
  }

  List(
    "A JSON-generated dLatch" -> latchCreatorFromJson,
    "A Verilog-generated dLatch" -> latchCreatorFromHdl
  ).foreach { case (desc, cc) =>
    s"$desc" should {
      "start unset" in {
        val (q, comp) = buildComponent { dLatch(cc, new Port(), new Port(), new Port()) }
        val sim = GateProcessor.setupAndRun(comp, Some(1000))
        sim.get(q) must beNone
      }

      "be set to the input when enable is high" in forAll { (in: LogicLevel) =>
        val (q, comp) = buildComponent { dLatch(cc, in, High, High) }
        val sim = GateProcessor.setupAndRun(comp, Some(1000))
        sim.get(q) must beSome(in.toBool)
      }

      "retain its original value when enable is low" in {
        val d, en = new Port()
        val (q, comp) = buildComponent { dLatch(cc, d, en, High) }

        runPlan(
          comp,
          25 -> { _.get(q) must beNone },
          50 -> { _.set(d, true) },
          75 -> { _.get(q) must beNone },
          100 -> { _.set(en, true) },
          150 -> { _.get(q) must beSome(true) },
          200 -> { _.set(en, false) },
          250 -> { _.set(d, false) },
          300 -> { _.get(q) must beSome(true) }
        )
      }

      "set the value to zero when rstn is low" in {
        val d, en, rstn = new Port()
        val (q, comp) = buildComponent { dLatch(cc, d, en, rstn) }

        runPlan(
          comp,
          10 -> { _.set(rstn, true) },
          50 -> { _.set(d, true) },
          100 -> { _.set(en, true) },
          150 -> { _.get(q) must beSome(true) },
          200 -> { _.set(rstn, false) },
          300 -> { _.get(q) must beSome(false) },
          350 -> { _.set(rstn, true) },
          400 -> { _.get(q) must beSome(true) },
          450 -> { _.set(en, false) },
          500 -> { _.set(rstn, false) },
          600 -> { _.get(q) must beSome(false) }
        )
      }

      "behave well under any combination of the above" in {
        val d, en, rstn = newPort()
        val (q, comp) = buildComponent { dLatch(cc, d, en, rstn) }

        var expectedQ = Option.empty[Boolean]

        SequentialScenario(comp)
          .withPorts(d -> false, en -> false, rstn -> true)
          .onStart { _ => expectedQ = None }
          .whenHigh(en) { sim => expectedQ = sim.get(d).orElse(expectedQ) }
          .whenLow(rstn) { sim => expectedQ = Some(false) }
          .check { sim => sim.get(q) must beEqualTo(expectedQ) }
          .run()
      }
    }
  }

  "A Verilog-generated tri-state bus with an inout port" should {
    "resolve the 2-driver shared bus through the wire group" in {
      val a, b, selA, selB = new Port()
      val bus: Bus = Vector(new Port())
      val (y, comp) = buildComponent {
        tristateY(tristateCreatorFromHdl, a, b, selA, selB, bus)
      }

      runPlan(
        comp,
        10 -> { _.set(a, true).set(selA, true) },
        30 -> { sim => sim.get(y) must beSome(true) },
        40 -> { sim => sim.get(bus.head) must beSome(true) },
        50 -> { _.set(selA, false) },
        70 -> { sim => sim.get(y) must beNone },
        80 -> { _.set(selB, true).set(b, false) },
        100 -> { sim => sim.get(y) must beSome(false) },
        110 -> { sim => sim.get(bus.head) must beSome(false) }
      )
    }

    "connect the inout port bidirectionally" in {
      val a, b, selA, selB = new Port()
      val bus: Bus = Vector(new Port())
      val (y, comp) = buildComponent {
        tristateY(tristateCreatorFromHdl, a, b, selA, selB, bus)
      }

      runPlan(
        comp,
        // both drivers floating: an external drive reaches the inside
        10 -> { _.set(bus.head, true) },
        30 -> { sim => sim.get(y) must beSome(true) },
        // an internal drive reaches the external port
        40 -> { _.unset(bus.head).set(selA, true).set(a, false) },
        60 -> { sim => sim.get(bus.head) must beSome(false) },
        70 -> { sim => sim.get(y) must beSome(false) }
      )
    }

    "register the inout port with Direction.Inout" in {
      val bus: Bus = Vector(new Port())
      val (_, comp) = buildComponent {
        tristateY(tristateCreatorFromHdl, new Port(), new Port(), new Port(), new Port(), bus)
      }
      val named = comp.asInstanceOf[CompositeComponent].namedPorts
      named("bus")._1 must beSome(Direction.Inout)
      named("y")._1 must beSome(Direction.Output)
      named("a")._1 must beSome(Direction.Input)
    }

    "keep RTL netnames on the built component" in {
      val bus: Bus = Vector(new Port())
      val (_, comp) = buildComponent {
        tristateY(tristateCreatorFromHdl, new Port(), new Port(), new Port(), new Port(), bus)
      }
      val idx = Index(comp.asInstanceOf[CompositeComponent])
      // visible netnames are registered as named ports/buses
      idx.ports.keys must contain("sel_a")
      idx.ports.keys must contain("bus")
    }
  }

  "A Verilog-generated hierarchical design" should {
    "keep submodule instances as named subcomponents with RTL netnames" in {
      val a, b = new Port()
      val (_, comp) = buildComponent {
        val outs = hierCreatorFromHdl.create(
          "topmod",
          Map("a" -> a, "b" -> b)
        )
        outs("y").asInstanceOf[Port]
      }
      val idx = Index(comp.asInstanceOf[CompositeComponent])
      // submodule instance keeps its RTL instance name
      idx.components.keys must contain("u1")
      // visible netnames inside the submodule are indexed under it...
      idx.ports.keys must contain("u1.internal_net")
      // ...while hidden (auto-generated) netnames are skipped: no port is named like one
      idx.ports.keys.forall(!_.split('.').last.startsWith("$")) must beTrue
    }
  }
}
