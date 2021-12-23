package component

import component.BuilderDSL._
import core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import simulator.{Circuit, Sim}
import testkit._

class LogicSpec extends BaseSpec {

  "A NOT" should {
    "compute !a" in forAll { (in: Option[LogicLevel]) =>
      val (out, state) = buildAndRun { implicit env => component.not(in.toPort) }
      state.get(out) must beEqualTo(in.map(!_.toBool))
    }
  }

  "An AND" should {
    "compute a & b" in forAll { (in1: Option[LogicLevel], in2: Option[LogicLevel]) =>
      val expected = (in1, in2) match {
        case (Some(Low), _) => Some(false)
        case (_, Some(Low)) => Some(false)
        case (Some(High), Some(High)) => Some(true)
        case _ => None
      }
      val (out, state) = buildAndRun { implicit env => and(in1.toPort, in2.toPort) }
      state.get(out) must beEqualTo(expected)
    }
  }

  "An OR" should {
    "compute a | b" in forAll { (in1: Option[LogicLevel], in2: Option[LogicLevel]) =>
      val expected = (in1, in2) match {
        case (Some(High), _) => Some(true)
        case (_, Some(High)) => Some(true)
        case (Some(Low), Some(Low)) => Some(false)
        case _ => None
      }
      val (out, state) = buildAndRun { implicit env => or(in1.toPort, in2.toPort) }
      state.get(out) must beEqualTo(expected)
    }
  }

  "A NOR" should {
    "compute !(a | b)" in forAll { (in1: Option[LogicLevel], in2: Option[LogicLevel]) =>
      val expected = (in1, in2) match {
        case (Some(High), _) => Some(false)
        case (_, Some(High)) => Some(false)
        case (Some(Low), Some(Low)) => Some(true)
        case _ => None
      }
      val (out, state) = buildAndRun { implicit env => nor(in1.toPort, in2.toPort) }
      state.get(out) must beEqualTo(expected)
    }
  }

  "A XOR" should {
    "compute a ^ b" in forAll { (in1: Option[LogicLevel], in2: Option[LogicLevel]) =>
      val expected = for { a <- in1; b <- in2 } yield a.toBool ^ b.toBool
      val (out, state) = buildAndRun { implicit env => xor(in1.toPort, in2.toPort) }
      state.get(out) must beEqualTo(expected)
    }
  }

  "A XNOR" should {
    "compute !(a ^ b)" in forAll { (in1: Option[LogicLevel], in2: Option[LogicLevel]) =>
      val expected = for { a <- in1; b <- in2 } yield !(a.toBool ^ b.toBool)
      val (out, state) = buildAndRun { implicit env => xnor(in1.toPort, in2.toPort) }
      state.get(out) must beEqualTo(expected)
    }
  }
}
