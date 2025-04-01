package yosys

import java.nio.file.Path

import component.BuilderAPI.*
import core.*
import org.scalacheck.Prop.forAll
import simulator.Sim
import testkit.*
import util.Implicits.*

class ComponentCreatorSpec extends BaseSpec with SequentialScenarios {
  val aoiCreator = ComponentCreator(Path.of(ClassLoader.getSystemResource("aoi.json").toURI()))
  val latchCreator = ComponentCreator(Path.of(ClassLoader.getSystemResource("dlatch.json").toURI()))

  def aoi(a: Port, b: Port, c: Port, d: Port): Spec[Port] = {
    val outs = aoiCreator.create("aoi", Map("A" -> a, "B" -> b, "C" -> c, "D" -> d))
    outs("F").asInstanceOf[Port]
  }

  def dLatch(d: Port, en: Port, rstn: Port): Spec[Port] = {
    val outs = latchCreator.create("d_latch", Map("d" -> d, "en" -> en, "rstn" -> rstn))
    outs("q").asInstanceOf[Port]
  }

  "An JSON-genrated AOI" should {
    "compute ~((a & b) | (c & d))" in forAll { (a: LogicLevel, b: LogicLevel, c: LogicLevel, d: LogicLevel) =>
      val expected = !((a.toBool && b.toBool) || (c.toBool && d.toBool))
      val (f, sim) = buildAndRun { aoi(a, b, c, d) }
      sim.get(f) must beSome(expected)
    }
  }

  "A JSON-genrated dLatch" should {

    "start unset" in {
      val (q, comp) = buildComponent { dLatch(new Port(), new Port(), new Port()) }
      val sim = Sim.setupAndRun(comp, Some(1000))
      sim.get(q) must beNone
    }

    "be set to the input when enable is high" in forAll { (in: LogicLevel) =>
      val (q, comp) = buildComponent { dLatch(in, High, High) }
      val sim = Sim.setupAndRun(comp, Some(1000))
      sim.get(q) must beSome(in.toBool)
    }

    "retain its original value when enable is low" in {
      val d, en = new Port()
      val (q, comp) = buildComponent { dLatch(d, en, High) }

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
      val (q, comp) = buildComponent { dLatch(d, en, rstn) }

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
      val (q, comp) = buildComponent { dLatch(d, en, rstn) }

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
