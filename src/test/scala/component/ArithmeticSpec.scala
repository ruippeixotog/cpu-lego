package component

import scala.collection.mutable.ListBuffer

import component.BuilderAPI.*
import core.*
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.execute.Result
import org.specs2.mutable.Specification
import simulator.{Circuit, GateProcessor}
import testkit.*
import util.Implicits.*

class ArithmeticSpec extends BaseSpec {

  given Arbitrary[Vector[(LogicLevel, LogicLevel)]] = Arbitrary(
    for {
      n <- Gen.choose(1, 20)
      xs <- Gen.listOfN(n, summon[Arbitrary[(LogicLevel, LogicLevel)]].arbitrary)
    } yield xs.toVector
  )

  "A fullAdder" should {
    "compute an addition with carry correctly" in forAll { (in1: LogicLevel, in2: LogicLevel, in3: LogicLevel) =>
      val ((out, carry), sim) = buildAndRun { fullAdder(in1, in2, in3) }

      val expected = List(in1, in2, in3).map(_.toInt).sum
      sim.get(out) must beSome(expected % 2 == 1)
      sim.get(carry) must beSome(expected / 2 == 1)
    }
  }

  "A binaryAdder" should {
    "compute an addition correctly" in forAll { (ins: Vector[(LogicLevel, LogicLevel)]) =>
      val (in1, in2) = ins.unzip
      val ((outs, carry), sim) = buildAndRun { binaryAdder(in1, in2) }

      sim.get(outs :+ carry).sequence must beSome.which { bools =>
        bools.toInt must beEqualTo(in1.toInt + in2.toInt)
      }
    }

    "compute an addition with carry in correctly" in forAll {
      (ins: Vector[(LogicLevel, LogicLevel)], carryIn: LogicLevel) =>
        val (in1, in2) = ins.unzip
        val ((outs, carryOut), sim) = buildAndRun { binaryAdder(in1, in2, carryIn) }

        sim.get(outs :+ carryOut).sequence must beSome.which { bools =>
          bools.toInt must beEqualTo(in1.toInt + in2.toInt + carryIn.toInt)
        }
    }

    "pass the carry through on empty buses" in forAll { (carryIn: LogicLevel) =>
      val ((outs, carryOut), sim) = buildAndRun { binaryAdder(Vector(), Vector(), carryIn) }
      outs must beEmpty
      sim.get(carryOut) must beSome(carryIn.toBool)
    }
  }

  "An addSub" should {

    "compute an addition correctly" in forAll { (ins: Vector[(LogicLevel, LogicLevel)]) =>
      val n = ins.length
      val (in1, in2) = ins.unzip
      val (outs, sim) = buildAndRun { addSub(in1, in2, Low) }

      sim.get(outs).sequence must beSome.which { bools =>
        bools.toInt.truncate(n) must beEqualTo((in1.toInt + in2.toInt).truncate(n))
      }
    }

    "compute a subtraction correctly" in forAll { (ins: Vector[(LogicLevel, LogicLevel)]) =>
      val n = ins.length
      val (in1, in2) = ins.unzip
      val (outs, sim) = buildAndRun { addSub(in1, in2, High) }

      sim.get(outs).sequence must beSome.which { bools =>
        bools.toInt.truncate(n) must beEqualTo((in1.toInt - in2.toInt).truncate(n))
      }
    }
  }

  // Exhaustive and property verification for the issue-33 arithmetic blocks.
  //
  // Each circuit is built once with drivable input ports and then driven through every input combination;
  // GateProcessor.run() settles the circuit before each read.

  /** Checks an adder with interface `(sum, cout, overflow)` against Scala integer arithmetic.
    *
    * `vectors` yields `(a, b, cin)` triples; expected results use Scala semantics (unsigned sum/carry, signed overflow
    * bounds).
    */
  private def checkAdder(
      adder: (Bus, Bus, Port) => Spec[(Bus, Port, Port)],
      n: Int,
      vectors: Seq[(Int, Int, Int)]
  ): List[String] = {
    val aIns = Vector.fill(n)(new Port)
    val bIns = Vector.fill(n)(new Port)
    val cin = new Port
    val ((sum, cout, overflow), comp) = buildComponent { adder(aIns, bIns, cin) }
    var sim = GateProcessor.setup(comp)
    val mask = (1 << n) - 1
    val bad = ListBuffer.empty[String]
    for ((a, b, c) <- vectors) {
      sim = sim.set(aIns, a.toBoolVec(n)).set(bIns, b.toBoolVec(n)).set(cin, c == 1).run()
      val total = a + b + c
      val signedTotal = a.toBoolVec(n).toSignedInt + b.toBoolVec(n).toSignedInt + c
      val expSum = total & mask
      val expCout = (total >>> n) == 1
      val expOverflow = signedTotal < -(1 << (n - 1)) || signedTotal >= (1 << (n - 1))
      val label = s"a=$a b=$b cin=$c"
      sim.get(sum).sequence match {
        case Some(bools) if bools.toInt != expSum =>
          bad += s"$label: sum ${bools.toInt} != $expSum"
        case None =>
          bad += s"$label: sum did not settle"
        case _ => ()
      }
      if (sim.get(cout) != Some(expCout)) bad += s"$label: cout ${sim.get(cout)} != $expCout"
      if (sim.get(overflow) != Some(expOverflow)) bad += s"$label: overflow ${sim.get(overflow)} != $expOverflow"
      if (bad.size >= 5) return bad.toList
    }
    bad.toList
  }

  private def allVectors(n: Int): Seq[(Int, Int, Int)] =
    for (a <- 0 to (1 << n) - 1; b <- 0 to (1 << n) - 1; c <- 0 to 1) yield (a, b, c)

  private def report(bad: List[String], count: Int): Result =
    if (bad.nonEmpty) failure(s"$count vectors, mismatches: ${bad.mkString("; ")}")
    else ok

  "A rippleAdder" should {
    "match Scala arithmetic on all 8-bit operand pairs" in {
      val vectors = allVectors(8)
      report(checkAdder(rippleAdder, 8, vectors), vectors.size)
    }

    "match Scala arithmetic on 16-bit random operands" in {
      val rng = new scala.util.Random(33)
      val vectors = Seq.fill(500) { (rng.nextInt(65536), rng.nextInt(65536), rng.nextInt(2)) }
      report(checkAdder(rippleAdder, 16, vectors), vectors.size)
    }

    "pass the carry through on empty buses" in {
      val ((sum, cout, overflow), sim) = buildAndRun { rippleAdder(Vector(), Vector(), High) }
      sum must beEmpty
      sim.get(cout) must beSome(true)
      sim.get(overflow) must beSome(false)
    }
  }

  "A prefixAdder" should {
    "match Scala arithmetic on all 8-bit operand pairs" in {
      val vectors = allVectors(8)
      report(checkAdder(prefixAdder, 8, vectors), vectors.size)
    }

    "match Scala arithmetic on 16-bit random operands" in {
      val rng = new scala.util.Random(95)
      val vectors = Seq.fill(500) { (rng.nextInt(65536), rng.nextInt(65536), rng.nextInt(2)) }
      report(checkAdder(prefixAdder, 16, vectors), vectors.size)
    }

    "agree with Scala arithmetic on odd widths" in {
      // Kogge-Stone must also work when the width is not a power of two.
      val rng = new scala.util.Random(7)
      for (n <- Seq(1, 3, 5, 12)) {
        val aIns = Vector.fill(n)(new Port)
        val bIns = Vector.fill(n)(new Port)
        val cin = new Port
        val ((sum, cout, overflow), comp) = buildComponent { prefixAdder(aIns, bIns, cin) }
        var sim = GateProcessor.setup(comp)
        for (_ <- 0 until 200) {
          val a = rng.nextInt(1 << n)
          val b = rng.nextInt(1 << n)
          val c = rng.nextInt(2)
          sim = sim.set(aIns, a.toBoolVec(n)).set(bIns, b.toBoolVec(n)).set(cin, c == 1).run()
          val total = a + b + c
          val signedTotal = a.toBoolVec(n).toSignedInt + b.toBoolVec(n).toSignedInt + c
          sim.get(sum).sequence must beSome.which { bools => bools.toInt must beEqualTo(total & ((1 << n) - 1)) }
          sim.get(cout) must beSome((total >>> n) == 1)
          sim.get(overflow) must beSome(signedTotal < -(1 << (n - 1)) || signedTotal >= (1 << (n - 1)))
        }
      }
      ok
    }

    "pass the carry through on empty buses" in {
      val ((sum, cout, overflow), sim) = buildAndRun { prefixAdder(Vector(), Vector(), High) }
      sum must beEmpty
      sim.get(cout) must beSome(true)
      sim.get(overflow) must beSome(false)
    }
  }

  "An incrementer" should {
    "add one when inc is high and pass through otherwise, for all 8-bit values" in {
      val n = 8
      val aIns = Vector.fill(n)(new Port)
      val inc = new Port
      val ((result, cout), comp) = buildComponent { incrementer(aIns, inc) }
      def collect(): List[String] = {
        var sim = GateProcessor.setup(comp)
        val bad = ListBuffer.empty[String]
        for (a <- 0 until 256; i <- 0 to 1) {
          sim = sim.set(aIns, a.toBoolVec(n)).set(inc, i == 1).run()
          val exp = if (i == 1) (a + 1) & 0xff else a
          val expCout = i == 1 && a == 255
          sim.get(result).sequence match {
            case Some(bools) if bools.toInt != exp => bad += s"a=$a inc=$i: result ${bools.toInt} != $exp"
            case None => bad += s"a=$a inc=$i: result did not settle"
            case _ => ()
          }
          if (sim.get(cout) != Some(expCout)) bad += s"a=$a inc=$i: cout ${sim.get(cout)} != $expCout"
          if (bad.size >= 5) return bad.toList
        }
        bad.toList
      }
      report(collect(), 512)
    }
  }

  "A decrementer" should {
    "subtract one when dec is high and pass through otherwise, for all 8-bit values" in {
      val n = 8
      val aIns = Vector.fill(n)(new Port)
      val dec = new Port
      val ((result, cout), comp) = buildComponent { decrementer(aIns, dec) }
      def collect(): List[String] = {
        var sim = GateProcessor.setup(comp)
        val bad = ListBuffer.empty[String]
        for (a <- 0 until 256; i <- 0 to 1) {
          sim = sim.set(aIns, a.toBoolVec(n)).set(dec, i == 1).run()
          val exp = if (i == 1) (a - 1) & 0xff else a
          // cout is the raw adder carry: High = no borrow; always Low when passing through.
          val expCout = i == 1 && a != 0
          sim.get(result).sequence match {
            case Some(bools) if bools.toInt != exp => bad += s"a=$a dec=$i: result ${bools.toInt} != $exp"
            case None => bad += s"a=$a dec=$i: result did not settle"
            case _ => ()
          }
          if (sim.get(cout) != Some(expCout)) bad += s"a=$a dec=$i: cout ${sim.get(cout)} != $expCout"
          if (bad.size >= 5) return bad.toList
        }
        bad.toList
      }
      report(collect(), 512)
    }
  }

  "A subtractor" should {
    "match Scala arithmetic on all 8-bit operand pairs" in {
      val n = 8
      val aIns = Vector.fill(n)(new Port)
      val bIns = Vector.fill(n)(new Port)
      val ((diff, cout, overflow), comp) = buildComponent { subtractor(aIns, bIns) }
      def collect(): (List[String], Int) = {
        var sim = GateProcessor.setup(comp)
        val bad = ListBuffer.empty[String]
        var count = 0
        for (a <- 0 until 256; b <- 0 until 256) {
          count += 1
          sim = sim.set(aIns, a.toBoolVec(n)).set(bIns, b.toBoolVec(n)).run()
          val expDiff = (a - b) & 0xff
          val expCout = a >= b // no borrow
          val sa = a.toBoolVec(n).toSignedInt
          val sb = b.toBoolVec(n).toSignedInt
          val expOverflow = sa - sb < -128 || sa - sb > 127
          val label = s"a=$a b=$b"
          sim.get(diff).sequence match {
            case Some(bools) if bools.toInt != expDiff => bad += s"$label: diff ${bools.toInt} != $expDiff"
            case None => bad += s"$label: diff did not settle"
            case _ => ()
          }
          if (sim.get(cout) != Some(expCout)) bad += s"$label: cout ${sim.get(cout)} != $expCout"
          if (sim.get(overflow) != Some(expOverflow)) bad += s"$label: overflow ${sim.get(overflow)} != $expOverflow"
          if (bad.size >= 5) return (bad.toList, count)
        }
        (bad.toList, count)
      }
      val (bad, count) = collect()
      report(bad, count)
    }
  }

  "A compare" should {
    "match Scala comparisons on all 8-bit operand pairs" in {
      val n = 8
      val aIns = Vector.fill(n)(new Port)
      val bIns = Vector.fill(n)(new Port)
      val ((eq, ltUnsigned, ltSigned), comp) = buildComponent { compare(aIns, bIns) }
      def collect(): (List[String], Int) = {
        var sim = GateProcessor.setup(comp)
        val bad = ListBuffer.empty[String]
        var count = 0
        for (a <- 0 until 256; b <- 0 until 256) {
          count += 1
          sim = sim.set(aIns, a.toBoolVec(n)).set(bIns, b.toBoolVec(n)).run()
          val sa = a.toBoolVec(n).toSignedInt
          val sb = b.toBoolVec(n).toSignedInt
          val label = s"a=$a b=$b"
          if (sim.get(eq) != Some(a == b)) bad += s"$label: eq ${sim.get(eq)} != ${a == b}"
          if (sim.get(ltUnsigned) != Some(a < b)) bad += s"$label: ltUnsigned ${sim.get(ltUnsigned)} != ${a < b}"
          if (sim.get(ltSigned) != Some(sa < sb)) bad += s"$label: ltSigned ${sim.get(ltSigned)} != ${sa < sb}"
          if (bad.size >= 5) return (bad.toList, count)
        }
        (bad.toList, count)
      }
      val (bad, count) = collect()
      report(bad, count)
    }

    "treat empty buses as equal" in {
      val ((eq, ltUnsigned, ltSigned), sim) = buildAndRun { compare(Vector(), Vector()) }
      sim.get(eq) must beSome(true)
      sim.get(ltUnsigned) must beSome(false)
      sim.get(ltSigned) must beSome(false)
    }
  }
}
