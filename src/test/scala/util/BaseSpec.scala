package util

import scala.annotation.targetName

import core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class BaseSpec extends Specification with ScalaCheck {

  val genLogicLevel: Gen[LogicLevel] = Gen.oneOf(High, Low)

  given Arbitrary[LogicLevel] = Arbitrary(genLogicLevel)

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
