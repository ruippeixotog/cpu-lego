package component

import scala.collection.mutable.ListBuffer

import component.BuilderAPI.*
import core.*
import org.specs2.execute.Result
import org.specs2.mutable.Specification
import simulator.GateProcessor
import testkit.*
import util.Implicits.*

class FlagsSpec extends BaseSpec {

  private def report(bad: List[String], count: Int): Result =
    if (bad.nonEmpty) failure(s"$count vectors, mismatches: ${bad.mkString("; ")}")
    else ok

  private def checkPort(
      sim: GateProcessor,
      got: Port,
      expected: Boolean,
      label: String,
      bad: ListBuffer[String]
  ): Unit =
    if (sim.get(got) != Some(expected) && bad.size < 5) bad += s"$label: ${sim.get(got)} != $expected"

  "flagN, flagZ and flagP" should {
    "match Scala bit tests for all 8-bit values" in {
      val n = 8
      val aIns = Vector.fill(n)(new Port)
      val ((neg, zero, parity), comp) = buildComponent {
        (flagN(aIns), flagZ(aIns), flagP(aIns))
      }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      for (a <- 0 until 256) {
        sim = sim.set(aIns, a.toBoolVec(n)).run()
        checkPort(sim, neg, (a & 0x80) != 0, s"a=$a N", bad)
        checkPort(sim, zero, a == 0, s"a=$a Z", bad)
        val ones = a.toBoolVec(n).count(identity)
        checkPort(sim, parity, ones % 2 == 0, s"a=$a P", bad)
      }
      report(bad.toList, 256)
    }

    "ignore bits above the low 8 for parity" in {
      val aIns = Vector.fill(16)(new Port)
      val (parity, comp) = buildComponent { flagP(aIns) }
      var sim = GateProcessor.setup(comp)
      // 0xFF00 has an even low byte (0 ones) but 8 high ones: P must be High.
      sim = sim.set(aIns, 0xff00.toBoolVec(16)).run()
      sim.get(parity) must beSome(true)
      // 0x0101 has one low one: P must be Low.
      sim = sim.set(aIns, 0x0101.toBoolVec(16)).run()
      sim.get(parity) must beSome(false)
    }

    "treat an empty bus as zero" in {
      val ((neg, zero, parity), sim) = buildAndRun { (flagN(Vector()), flagZ(Vector()), flagP(Vector())) }
      sim.get(neg) must beSome(false)
      sim.get(zero) must beSome(true)
      sim.get(parity) must beSome(true)
    }
  }

  "flagVAdd" should {
    "match signed-overflow arithmetic for all 8-bit operand pairs" in {
      val n = 8
      val aIns = Vector.fill(n)(new Port)
      val bIns = Vector.fill(n)(new Port)
      val sumIns = Vector.fill(n)(new Port)
      val (v, comp) = buildComponent { flagVAdd(aIns, bIns, sumIns) }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      for (a <- 0 until 256; b <- 0 until 256) {
        val sum = (a + b) & 0xff
        sim = sim.set(aIns, a.toBoolVec(n)).set(bIns, b.toBoolVec(n)).set(sumIns, sum.toBoolVec(n)).run()
        val sa = a.toBoolVec(n).toSignedInt
        val sb = b.toBoolVec(n).toSignedInt
        checkPort(sim, v, sa + sb < -128 || sa + sb > 127, s"a=$a b=$b V", bad)
      }
      report(bad.toList, 65536)
    }
  }

  "flagVSub" should {
    "match signed-overflow arithmetic for all 8-bit operand pairs" in {
      val n = 8
      val aIns = Vector.fill(n)(new Port)
      val bIns = Vector.fill(n)(new Port)
      val diffIns = Vector.fill(n)(new Port)
      val (v, comp) = buildComponent { flagVSub(aIns, bIns, diffIns) }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      for (a <- 0 until 256; b <- 0 until 256) {
        val diff = (a - b) & 0xff
        sim = sim.set(aIns, a.toBoolVec(n)).set(bIns, b.toBoolVec(n)).set(diffIns, diff.toBoolVec(n)).run()
        val sa = a.toBoolVec(n).toSignedInt
        val sb = b.toBoolVec(n).toSignedInt
        checkPort(sim, v, sa - sb < -128 || sa - sb > 127, s"a=$a b=$b V", bad)
      }
      report(bad.toList, 65536)
    }
  }

  "flagAFAdd" should {
    "be the carry out of bit 3 for all 8-bit operand pairs and carry-ins" in {
      val n = 8
      val aIns = Vector.fill(n)(new Port)
      val bIns = Vector.fill(n)(new Port)
      val cin = new Port
      val (af, comp) = buildComponent { flagAFAdd(aIns, bIns, cin) }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      for (a <- 0 until 256; b <- 0 until 256; c <- 0 to 1) {
        sim = sim.set(aIns, a.toBoolVec(n)).set(bIns, b.toBoolVec(n)).set(cin, c == 1).run()
        checkPort(sim, af, (a & 0xf) + (b & 0xf) + c > 15, s"a=$a b=$b cin=$c AF", bad)
      }
      report(bad.toList, 131072)
    }
  }

  "flagAFSub" should {
    "be the borrow out of bit 3 for all 8-bit operand pairs and borrows" in {
      val n = 8
      val aIns = Vector.fill(n)(new Port)
      val bIns = Vector.fill(n)(new Port)
      val borrow = new Port
      val (af, comp) = buildComponent { flagAFSub(aIns, bIns, borrow) }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      for (a <- 0 until 256; b <- 0 until 256; br <- 0 to 1) {
        sim = sim.set(aIns, a.toBoolVec(n)).set(bIns, b.toBoolVec(n)).set(borrow, br == 1).run()
        checkPort(sim, af, (a & 0xf) - (b & 0xf) - br < 0, s"a=$a b=$b borrow=$br AF", bad)
      }
      report(bad.toList, 131072)
    }
  }
}
