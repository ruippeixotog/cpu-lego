package testkit

import scala.annotation.targetName

import component.BuilderAPI._
import core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.execute.{AsResult, Result}
import org.specs2.mutable.Specification
import simulator.{Sim, SimState}

abstract class BaseSpec extends Specification with ScalaCheck {

  // --- ScalaCheck generators ---

  val genLogicLevel: Gen[LogicLevel] = Gen.oneOf(High, Low)

  given Arbitrary[LogicLevel] = Arbitrary(genLogicLevel)

  // --- Utility methods ---

  def buildAndRun[A](spec: Spec[A]): (A, SimState) = {
    val (res, comp) = buildComponent(spec)
    (res, Sim.runComponent(comp))
  }

  def runPlan(comp: Component, plan: (Int, SimState => Result | Unit)*): Result = {
    var state = Sim.setup(Sim.build(comp))
    foreach(plan.toList.sortBy(_._1)) { case (tick, f) =>
      state = Sim.run(state, Some(tick))
      f(state) match {
        case r: Result => r.updateMessage(s"At t=$tick: " + _)
        case () => ok
      }
    }
  }

  def foreachTick(comp: Component, maxTicks: Int)(f: (Int, SimState) => Result): Result = {
    runPlan(comp, (0 to maxTicks).map { tick => (tick, f(tick, _)) }: _*)
  }

  // --- Extension methods ---

  extension [A](self: Seq[Option[A]]) {
    def sequence: Option[Seq[A]] =
      self.foldRight[Option[List[A]]](Some(Nil)) { case (x, acc) => x.zip(acc).map(_ :: _) }
  }

  extension (self: LogicLevel) {
    def toBool: Boolean = self match {
      case High => true
      case Low => false
    }

    def toInt: Int = self match {
      case High => 1
      case Low => 0
    }
  }

  extension (self: Option[LogicLevel]) {
    def toPort: Port = self.getOrElse(new Port)
  }

  extension (self: Seq[LogicLevel]) {
    def toInt: Int = self.map(_.toBool).toInt
  }

  extension (self: Seq[Boolean]) {
    @targetName("boolSeqToUInt")
    def toInt: Int = self.zipWithIndex.filter(_._1).foldLeft(0)((acc, i) => acc | (1 << i._2))
  }

  extension (self: Int) {
    def truncate(n: Int): Int = self & (1 << n - 1)
  }
}
