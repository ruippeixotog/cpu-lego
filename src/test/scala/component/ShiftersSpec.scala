package component

import scala.collection.mutable.ListBuffer

import component.BuilderAPI.*
import core.*
import org.specs2.execute.Result
import org.specs2.mutable.Specification
import simulator.GateProcessor
import testkit.*
import util.Implicits.*

class ShiftersSpec extends BaseSpec {

  private def report(bad: List[String], count: Int): Result =
    if (bad.nonEmpty) failure(s"$count vectors, mismatches: ${bad.mkString("; ")}")
    else ok

  private def checkBus(
      sim: GateProcessor,
      got: Bus,
      expected: Int,
      n: Int,
      label: String,
      bad: ListBuffer[String]
  ): Unit =
    sim.get(got).sequence match {
      case Some(bools) if bools.toInt != expected && bad.size < 5 => bad += s"$label: ${bools.toInt} != $expected"
      case None if bad.size < 5 => bad += s"$label: did not settle"
      case _ => ()
    }

  private def checkPort(
      sim: GateProcessor,
      got: Port,
      expected: Boolean,
      label: String,
      bad: ListBuffer[String]
  ): Unit =
    if (sim.get(got) != Some(expected) && bad.size < 5) bad += s"$label: ${sim.get(got)} != $expected"

  "An asl" should {
    "shift left by one, carrying out the top bit, for all 8-bit values" in {
      val n = 8
      val aIns = Vector.fill(n)(new Port)
      val ((result, cout), comp) = buildComponent { asl(aIns) }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      for (a <- 0 until 256) {
        sim = sim.set(aIns, a.toBoolVec(n)).run()
        checkBus(sim, result, (a << 1) & 0xff, n, s"a=$a", bad)
        checkPort(sim, cout, (a & 0x80) != 0, s"a=$a cout", bad)
      }
      report(bad.toList, 256)
    }

    "pass an empty bus through with cout Low" in {
      val ((result, cout), sim) = buildAndRun { asl(Vector()) }
      result must beEmpty
      sim.get(cout) must beSome(false)
    }
  }

  "An lsr" should {
    "shift right by one, carrying out the bottom bit, for all 8-bit values" in {
      val n = 8
      val aIns = Vector.fill(n)(new Port)
      val ((result, cout), comp) = buildComponent { lsr(aIns) }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      for (a <- 0 until 256) {
        sim = sim.set(aIns, a.toBoolVec(n)).run()
        checkBus(sim, result, a >>> 1, n, s"a=$a", bad)
        checkPort(sim, cout, (a & 1) != 0, s"a=$a cout", bad)
      }
      report(bad.toList, 256)
    }

    "pass an empty bus through with cout Low" in {
      val ((result, cout), sim) = buildAndRun { lsr(Vector()) }
      result must beEmpty
      sim.get(cout) must beSome(false)
    }
  }

  "A rol" should {
    "rotate left through carry for all 8-bit values and carry-ins" in {
      val n = 8
      val aIns = Vector.fill(n)(new Port)
      val cin = new Port
      val ((result, cout), comp) = buildComponent { rol(aIns, cin) }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      for (a <- 0 until 256; c <- 0 to 1) {
        sim = sim.set(aIns, a.toBoolVec(n)).set(cin, c == 1).run()
        val expected = ((a << 1) & 0xff) | c
        checkBus(sim, result, expected, n, s"a=$a cin=$c", bad)
        checkPort(sim, cout, (a & 0x80) != 0, s"a=$a cin=$c cout", bad)
      }
      report(bad.toList, 512)
    }
  }

  "A ror" should {
    "rotate right through carry for all 8-bit values and carry-ins" in {
      val n = 8
      val aIns = Vector.fill(n)(new Port)
      val cin = new Port
      val ((result, cout), comp) = buildComponent { ror(aIns, cin) }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      for (a <- 0 until 256; c <- 0 to 1) {
        sim = sim.set(aIns, a.toBoolVec(n)).set(cin, c == 1).run()
        val expected = (a >>> 1) | (c << 7)
        checkBus(sim, result, expected, n, s"a=$a cin=$c", bad)
        checkPort(sim, cout, (a & 1) != 0, s"a=$a cin=$c cout", bad)
      }
      report(bad.toList, 512)
    }
  }

  private def checkBarrel(
      op: (Bus, Bus) => Spec[(Bus, Port)],
      ref: (Int, Int, Int) => (Int, Boolean),
      n: Int,
      amountBits: Int
  ): Result = {
    val aIns = Vector.fill(n)(new Port)
    val amountIns = Vector.fill(amountBits)(new Port)
    val ((result, cout), comp) = buildComponent { op(aIns, amountIns) }
    var sim = GateProcessor.setup(comp)
    val bad = ListBuffer.empty[String]
    var count = 0
    for (a <- 0 until (1 << n); k <- 0 until (1 << amountBits)) {
      count += 1
      sim = sim.set(aIns, a.toBoolVec(n)).set(amountIns, k.toBoolVec(amountBits)).run()
      val (expected, expCout) = ref(a, k, n)
      checkBus(sim, result, expected, n, s"a=$a k=$k", bad)
      checkPort(sim, cout, expCout, s"a=$a k=$k cout", bad)
      if (bad.size >= 5) return report(bad.toList, count)
    }
    report(bad.toList, count)
  }

  private def shiftLeftRef(a: Int, k: Int, n: Int): (Int, Boolean) = {
    val mask = (1 << n) - 1
    val result = if (k >= n) 0 else (a << k) & mask
    val cout = k >= 1 && k <= n && ((a >>> (n - k)) & 1) == 1
    (result, cout)
  }

  private def shiftRightRef(a: Int, k: Int, n: Int): (Int, Boolean) = {
    val result = if (k >= n) 0 else a >>> k
    val cout = k >= 1 && k <= n && ((a >>> (k - 1)) & 1) == 1
    (result, cout)
  }

  private def shiftRightArithmeticRef(a: Int, k: Int, n: Int): (Int, Boolean) = {
    val mask = (1 << n) - 1
    val sign = (a >>> (n - 1)) & 1
    val fill = if (sign == 1) (mask << (n - k)) & mask else 0
    val result = if (k >= n) (if (sign == 1) mask else 0) else (a >>> k) | fill
    val cout = k >= 1 && k <= n && ((a >>> (k - 1)) & 1) == 1
    (result, cout)
  }

  private def rotateLeftRef(a: Int, k: Int, n: Int): (Int, Boolean) = {
    val mask = (1 << n) - 1
    val r = k % n
    val result = if (r == 0) a else (((a << r) & mask) | (a >>> (n - r)))
    (result, ((result >>> (n - 1)) & 1) == 1)
  }

  private def rotateRightRef(a: Int, k: Int, n: Int): (Int, Boolean) = {
    val r = k % n
    val result = if (r == 0) a else (a >>> r) | ((a << (n - r)) & ((1 << n) - 1))
    (result, (result & 1) == 1)
  }

  "A barrelShiftLeft" should {
    "shift left by 0..31 for all 8-bit values" in {
      checkBarrel(barrelShiftLeft, shiftLeftRef, 8, 5)
    }
  }

  "A barrelShiftRight" should {
    "shift right by 0..31 for all 8-bit values" in {
      checkBarrel(barrelShiftRight, shiftRightRef, 8, 5)
    }
  }

  "A barrelShiftRightArithmetic" should {
    "shift right arithmetically by 0..31 for all 8-bit values" in {
      checkBarrel(barrelShiftRightArithmetic, shiftRightArithmeticRef, 8, 5)
    }
  }

  "A barrelRotateLeft" should {
    "rotate left by 0..31 for all 8-bit values" in {
      checkBarrel(barrelRotateLeft, rotateLeftRef, 8, 5)
    }
  }

  "A barrelRotateRight" should {
    "rotate right by 0..31 for all 8-bit values" in {
      checkBarrel(barrelRotateRight, rotateRightRef, 8, 5)
    }
  }

  "The barrel shifters" should {
    "agree with Scala arithmetic on random 16-bit values" in {
      val rng = new scala.util.Random(34)
      val n = 16
      val aIns = Vector.fill(n)(new Port)
      val amountIns = Vector.fill(5)(new Port)
      val ops: Seq[(String, (Bus, Bus) => Spec[(Bus, Port)], (Int, Int, Int) => (Int, Boolean))] = Seq(
        ("shl", barrelShiftLeft, shiftLeftRef),
        ("shr", barrelShiftRight, shiftRightRef),
        ("sar", barrelShiftRightArithmetic, shiftRightArithmeticRef),
        ("rol", barrelRotateLeft, rotateLeftRef),
        ("ror", barrelRotateRight, rotateRightRef)
      )
      val bad = ListBuffer.empty[String]
      for ((name, op, ref) <- ops) {
        val ((result, cout), comp) = buildComponent { op(aIns, amountIns) }
        var sim = GateProcessor.setup(comp)
        for (_ <- 0 until 500) {
          val a = rng.nextInt(1 << n)
          val k = rng.nextInt(32)
          sim = sim.set(aIns, a.toBoolVec(n)).set(amountIns, k.toBoolVec(5)).run()
          val (expected, expCout) = ref(a, k, n)
          checkBus(sim, result, expected, n, s"$name a=$a k=$k", bad)
          checkPort(sim, cout, expCout, s"$name a=$a k=$k cout", bad)
        }
      }
      report(bad.toList, 2500)
    }

    "pass empty buses through with cout Low" in {
      val ((r1, c1), sim1) = buildAndRun { barrelShiftLeft(Vector(), Vector()) }
      r1 must beEmpty
      sim1.get(c1) must beSome(false)
      val ((r2, c2), sim2) = buildAndRun { barrelRotateRight(Vector(), Vector()) }
      r2 must beEmpty
      sim2.get(c2) must beSome(false)
    }
  }
}
