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
    def toLittleEndianInt: Int = self.map(_.toBool).toLittleEndianInt
  }

  extension (self: Seq[Boolean]) {
    @targetName("boolToLittleEndianInt")
    def toLittleEndianInt: Int = self.zipWithIndex.map { case (b, idx) => (if (b) 1 else 0) << idx }.sum
  }
}
