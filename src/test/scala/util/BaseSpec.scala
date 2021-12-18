package util

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
    def toInt = self match {
      case High => 1
      case Low => 0
    }
  }
}
