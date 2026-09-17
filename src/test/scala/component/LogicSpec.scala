package component

import component.BuilderAPI.*
import core.*
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import simulator.{Circuit, Sim}
import testkit.*
import util.Implicits.*

class LogicSpec extends BaseSpec {

  "A NOT" should {
    "compute !a" in forAll { (in: Option[LogicLevel]) =>
      val (out, sim) = buildAndRun { component.not(in.toPort) }
      sim.get(out) must beEqualTo(in.map(!_.toBool))
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
      val (out, sim) = buildAndRun { and(in1.toPort, in2.toPort) }
      sim.get(out) must beEqualTo(expected)
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
      val (out, sim) = buildAndRun { or(in1.toPort, in2.toPort) }
      sim.get(out) must beEqualTo(expected)
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
      val (out, sim) = buildAndRun { nor(in1.toPort, in2.toPort) }
      sim.get(out) must beEqualTo(expected)
    }
  }

  "A XOR" should {
    "compute a ^ b" in forAll { (in1: Option[LogicLevel], in2: Option[LogicLevel]) =>
      val expected = for { a <- in1; b <- in2 } yield a.toBool ^ b.toBool
      val (out, sim) = buildAndRun { xor(in1.toPort, in2.toPort) }
      sim.get(out) must beEqualTo(expected)
    }
  }

  "A XNOR" should {
    "compute !(a ^ b)" in forAll { (in1: Option[LogicLevel], in2: Option[LogicLevel]) =>
      val expected = for { a <- in1; b <- in2 } yield !(a.toBool ^ b.toBool)
      val (out, sim) = buildAndRun { xnor(in1.toPort, in2.toPort) }
      sim.get(out) must beEqualTo(expected)
    }
  }

  "An andM" should {
    "compute the conjunction of all inputs" in forAll(Gen.choose(1, 6)) { n =>
      forAll(Gen.listOfN(n, summon[Arbitrary[LogicLevel]].arbitrary)) { ins =>
        val (out, sim) = buildAndRun { andM(ins*) }
        sim.get(out) must beSome(ins.forall(_ == High))
      }
    }

    "propagate unset inputs with Kleene semantics" in forAll(Gen.choose(1, 6)) { n =>
      forAll(Gen.listOfN(n, summon[Arbitrary[Option[LogicLevel]]].arbitrary)) { ins =>
        val expected =
          if (ins.contains(Some(Low))) Some(false)
          else if (ins.forall(_ == Some(High))) Some(true)
          else None
        val (out, sim) = buildAndRun { andM(ins.map(_.toPort)*) }
        sim.get(out) must beEqualTo(expected)
      }
    }

    "behave as the identity on a single input" in forAll { (in: LogicLevel) =>
      val (out, sim) = buildAndRun { andM(in) }
      sim.get(out) must beSome(in.toBool)
    }

    "throw on an empty input list" in {
      buildAndRun { andM() } must throwA[UnsupportedOperationException]
    }
  }

  "A nandM" should {
    "compute the negation of the conjunction of all inputs" in forAll(Gen.choose(1, 6)) { n =>
      forAll(Gen.listOfN(n, summon[Arbitrary[LogicLevel]].arbitrary)) { ins =>
        val (out, sim) = buildAndRun { nandM(ins*) }
        sim.get(out) must beSome(!ins.forall(_ == High))
      }
    }

    "propagate unset inputs with Kleene semantics" in forAll(Gen.choose(1, 6)) { n =>
      forAll(Gen.listOfN(n, summon[Arbitrary[Option[LogicLevel]]].arbitrary)) { ins =>
        val expected =
          if (ins.contains(Some(Low))) Some(true)
          else if (ins.forall(_ == Some(High))) Some(false)
          else None
        val (out, sim) = buildAndRun { nandM(ins.map(_.toPort)*) }
        sim.get(out) must beEqualTo(expected)
      }
    }

    "behave as a NOT on a single input" in forAll { (in: LogicLevel) =>
      val (out, sim) = buildAndRun { nandM(in) }
      sim.get(out) must beSome(!in.toBool)
    }
  }

  "An orM" should {
    "compute the disjunction of all inputs" in forAll(Gen.choose(1, 6)) { n =>
      forAll(Gen.listOfN(n, summon[Arbitrary[LogicLevel]].arbitrary)) { ins =>
        val (out, sim) = buildAndRun { orM(ins*) }
        sim.get(out) must beSome(ins.contains(High))
      }
    }

    "propagate unset inputs with Kleene semantics" in forAll(Gen.choose(1, 6)) { n =>
      forAll(Gen.listOfN(n, summon[Arbitrary[Option[LogicLevel]]].arbitrary)) { ins =>
        val expected =
          if (ins.contains(Some(High))) Some(true)
          else if (ins.forall(_ == Some(Low))) Some(false)
          else None
        val (out, sim) = buildAndRun { orM(ins.map(_.toPort)*) }
        sim.get(out) must beEqualTo(expected)
      }
    }

    "behave as the identity on a single input" in forAll { (in: LogicLevel) =>
      val (out, sim) = buildAndRun { orM(in) }
      sim.get(out) must beSome(in.toBool)
    }
  }

  "A norM" should {
    "compute the negation of the disjunction of all inputs" in forAll(Gen.choose(1, 6)) { n =>
      forAll(Gen.listOfN(n, summon[Arbitrary[LogicLevel]].arbitrary)) { ins =>
        val (out, sim) = buildAndRun { norM(ins*) }
        sim.get(out) must beSome(!ins.contains(High))
      }
    }

    "propagate unset inputs with Kleene semantics" in forAll(Gen.choose(1, 6)) { n =>
      forAll(Gen.listOfN(n, summon[Arbitrary[Option[LogicLevel]]].arbitrary)) { ins =>
        val expected =
          if (ins.contains(Some(High))) Some(false)
          else if (ins.forall(_ == Some(Low))) Some(true)
          else None
        val (out, sim) = buildAndRun { norM(ins.map(_.toPort)*) }
        sim.get(out) must beEqualTo(expected)
      }
    }

    "behave as a NOT on a single input" in forAll { (in: LogicLevel) =>
      val (out, sim) = buildAndRun { norM(in) }
      sim.get(out) must beSome(!in.toBool)
    }
  }

  "A xorM" should {
    "compute the parity of all inputs" in forAll(Gen.choose(1, 6)) { n =>
      forAll(Gen.listOfN(n, summon[Arbitrary[LogicLevel]].arbitrary)) { ins =>
        val (out, sim) = buildAndRun { xorM(ins*) }
        sim.get(out) must beSome(ins.map(_.toBool).reduce(_ ^ _))
      }
    }

    "stay unset unless every input is driven" in forAll(Gen.choose(1, 6)) { n =>
      forAll(Gen.listOfN(n, summon[Arbitrary[Option[LogicLevel]]].arbitrary)) { ins =>
        val expected =
          if (ins.forall(_.isDefined)) Some(ins.flatten.map(_.toBool).reduce(_ ^ _))
          else None
        val (out, sim) = buildAndRun { xorM(ins.map(_.toPort)*) }
        sim.get(out) must beEqualTo(expected)
      }
    }

    "behave as the identity on a single input" in forAll { (in: LogicLevel) =>
      val (out, sim) = buildAndRun { xorM(in) }
      sim.get(out) must beSome(in.toBool)
    }
  }

  "A xnorM" should {
    "compute the negated parity of all inputs" in forAll(Gen.choose(1, 6)) { n =>
      forAll(Gen.listOfN(n, summon[Arbitrary[LogicLevel]].arbitrary)) { ins =>
        val (out, sim) = buildAndRun { xnorM(ins*) }
        sim.get(out) must beSome(!ins.map(_.toBool).reduce(_ ^ _))
      }
    }

    "stay unset unless every input is driven" in forAll(Gen.choose(1, 6)) { n =>
      forAll(Gen.listOfN(n, summon[Arbitrary[Option[LogicLevel]]].arbitrary)) { ins =>
        val expected =
          if (ins.forall(_.isDefined)) Some(!ins.flatten.map(_.toBool).reduce(_ ^ _))
          else None
        val (out, sim) = buildAndRun { xnorM(ins.map(_.toPort)*) }
        sim.get(out) must beEqualTo(expected)
      }
    }

    "behave as a NOT on a single input" in forAll { (in: LogicLevel) =>
      val (out, sim) = buildAndRun { xnorM(in) }
      sim.get(out) must beSome(!in.toBool)
    }
  }
}
