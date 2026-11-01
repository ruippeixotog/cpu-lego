package testkit

import scala.annotation.targetName
import scala.collection.mutable.ListBuffer

import component.BuilderAPI.*
import core.*
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.execute.{AsResult, Result}
import org.specs2.mutable.Specification
import simulator.GateProcessor
import util.Implicits.*

abstract class BaseSpec extends Specification with ScalaCheck {

  // --- ScalaCheck generators ---

  val genLogicLevel: Gen[LogicLevel] = Gen.oneOf(High, Low)

  given Arbitrary[LogicLevel] = Arbitrary(genLogicLevel)

  // --- Utility methods ---

  /** Tests pick widths at runtime: raw vectors become dynamic buses, whose widths are checked by the component asserts.
    */
  given vectorToBus[P <: Port]: Conversion[Vector[P], Bus[Int]] = v => Bus.dynamic(v)

  def buildAndRun[A](spec: Spec[A]): (A, GateProcessor) = {
    val (res, comp) = buildComponent(spec)
    (res, GateProcessor.setupAndRun(comp))
  }

  def runPlan(comp: Component, plan: (Int, GateProcessor => Result | GateProcessor)*): Result = {
    var sim = GateProcessor.setup(comp)
    foreach(plan.toList.sortBy(_._1)) { case (tick, f) =>
      sim = sim.run(Some(tick))
      f(sim) match {
        case r: Result => r.updateMessage(s"At t=$tick: " + _)
        case sim1: GateProcessor => sim = sim1; ok
      }
    }
  }

  def foreachTick(comp: Component, fromTick: Int, toTick: Int)(f: (Int, GateProcessor) => Result): Result = {
    runPlan(comp, (fromTick to toTick).map { tick => (tick, f(tick, _)) }*)
  }

  // --- Shared spec helpers (used by the component specs) ---

  def report(bad: List[String], count: Int): Result =
    if (bad.nonEmpty) failure(s"$count vectors, mismatches: ${bad.mkString("; ")}")
    else ok

  def checkBus(
      sim: GateProcessor,
      got: Bus,
      expected: Int,
      label: String,
      bad: ListBuffer[String]
  ): Unit =
    sim.get(got).sequence match {
      case Some(bools) if bools.toInt != expected && bad.size < 5 => bad += s"$label: ${bools.toInt} != $expected"
      case None if bad.size < 5 => bad += s"$label: did not settle"
      case _ => ()
    }

  def checkPort(
      sim: GateProcessor,
      got: Port,
      expected: Boolean,
      label: String,
      bad: ListBuffer[String]
  ): Unit =
    if (sim.get(got) != Some(expected) && bad.size < 5) bad += s"$label: ${sim.get(got)} != $expected"

  // --- Extension methods ---

  extension (self: Int) {
    def truncate(n: Int): Int = self & (1 << n - 1)
  }
}
