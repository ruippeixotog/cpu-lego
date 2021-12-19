package util

import scala.annotation.targetName

import component.BuilderDSL._
import core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import simulator.{Sim, SimState}

class BaseSpec extends Specification with ScalaCheck {

  // --- ScalaCheck generators ---

  val genLogicLevel: Gen[LogicLevel] = Gen.oneOf(High, Low)

  given Arbitrary[LogicLevel] = Arbitrary(genLogicLevel)

  // --- Utility methods ---

  def buildAndRun[A](buildFunc: BuilderEnv => A): (A, SimState) = {
    val (res, comp) = buildComponent(buildFunc)
    (res, Sim.runComponent(comp))
  }

  // --- Extension methods ---

  extension [A](self: Seq[Option[A]]) {
    def sequence: Option[Seq[A]] =
      self.foldLeft[Option[List[A]]](Some(Nil)) { case (acc, x) => x.zip(acc).map(_ :: _) }
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
    def toUInt: Int = self.map(_.toBool).toUInt
    def toInt: Int = self.map(_.toBool).toInt
  }

  extension (self: Seq[Boolean]) {
    @targetName("boolSeqToUInt")
    def toUInt: Int = self.zipWithIndex.filter(_._1).foldLeft(0)((acc, i) => acc & (1 << i._2))

    @targetName("boolSeqToInt")
    def toInt: Int = (self ++ List.fill(32 - self.length)(self.last)).toUInt
  }
}
