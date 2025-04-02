package yosys

import java.nio.file.{Path, Paths}

import component.BuilderAPI.*
import core.*
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import simulator.Sim
import testkit.*
import util.Implicits.*

class ComponentCreatorSpec extends BaseSpec with SequentialScenarios {
  def resourcePath(res: String) = Path.of(getClass.getResource("/" + res).toURI)

  val aoiCreatorFromJson = ComponentCreator.fromYosysJsonFile(resourcePath("aoi.json"))
  val latchCreatorFromJson = ComponentCreator.fromYosysJsonFile(resourcePath("dlatch.json"))
  val aoiCreatorFromHdl = ComponentCreator.fromVerilog(resourcePath("aoi.v"))
  val latchCreatorFromHdl = ComponentCreator.fromVerilog(resourcePath("dlatch.v"))

  def aoi(cc: ComponentCreator, a: Port, b: Port, c: Port, d: Port): Spec[Port] = {
    val outs = cc.create("aoi", Map("A" -> a, "B" -> b, "C" -> c, "D" -> d))
    outs("F").asInstanceOf[Port]
  }

  def dLatch(cc: ComponentCreator, d: Port, en: Port, rstn: Port): Spec[Port] = {
    val outs = cc.create("d_latch", Map("d" -> d, "en" -> en, "rstn" -> rstn))
    outs("q").asInstanceOf[Port]
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
        val sim = Sim.setupAndRun(comp, Some(1000))
        sim.get(q) must beNone
      }

      "be set to the input when enable is high" in forAll { (in: LogicLevel) =>
        val (q, comp) = buildComponent { dLatch(cc, in, High, High) }
        val sim = Sim.setupAndRun(comp, Some(1000))
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
          .withPorts(d, en -> false, rstn -> true)
          .onStart { _ => expectedQ = None }
          .whenHigh(en) { sim => expectedQ = sim.get(d).orElse(expectedQ) }
          .whenLow(rstn) { sim => expectedQ = Some(false) }
          .check { sim => sim.get(q) must beEqualTo(expectedQ) }
          .run()
      }
    }
  }
}
