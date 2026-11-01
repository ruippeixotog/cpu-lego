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
      checkBus(sim, result, expected, s"a=$a k=$k", bad)
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
    val cout = if (k == 0) false else if (k >= n) sign == 1 else ((a >>> (k - 1)) & 1) == 1
    (result, cout)
  }

  private def rotateLeftRef(a: Int, k: Int, n: Int): (Int, Boolean) = {
    val mask = (1 << n) - 1
    val r = k % n
    val result = if (r == 0) a else (((a << r) & mask) | (a >>> (n - r)))
    (result, (result & 1) == 1)
  }

  private def rotateRightRef(a: Int, k: Int, n: Int): (Int, Boolean) = {
    val r = k % n
    val result = if (r == 0) a else (a >>> r) | ((a << (n - r)) & ((1 << n) - 1))
    (result, ((result >>> (n - 1)) & 1) == 1)
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
          checkBus(sim, result, expected, s"$name a=$a k=$k", bad)
          checkPort(sim, cout, expCout, s"$name a=$a k=$k cout", bad)
        }
      }
      report(bad.toList, 2500)
    }
  }

  // Hardware vectors from SingleStepTests/8088 (D0.0, D0.1, D2.0, D2.1, D2.7),
  // checked against the circuits directly rather than through the Scala refs.
  // Rotate counts of 0 are excluded: the 8086 leaves the flags unchanged for a
  // zero count, which a cout-returning function cannot express.
  "The barrel rotates and SAR" should {
    "match SingleStepTests/8088 hardware vectors" in {
      val bad = ListBuffer.empty[String]
      var count = 0
      def check(
          op: (Bus, Bus) => Spec[(Bus, Port)],
          vectors: Seq[(Int, Int, Int, Boolean)],
          amountBits: Int
      ): Unit = {
        val aIns = Vector.fill(8)(new Port)
        val amountIns = Vector.fill(amountBits)(new Port)
        val ((result, cout), comp) = buildComponent { op(aIns, amountIns) }
        var sim = GateProcessor.setup(comp)
        for ((a, k, r, c) <- vectors) {
          count += 1
          sim = sim.set(aIns, a.toBoolVec(8)).set(amountIns, k.toBoolVec(amountBits)).run()
          checkBus(sim, result, r, s"a=$a k=$k", bad)
          checkPort(sim, cout, c, s"a=$a k=$k cout", bad)
        }
      }
      // ROL by 1 (D0.0), including 0x98.
      check(
        barrelRotateLeft,
        Seq(
          (0x3e, 1, 0x7c, false),
          (0xfc, 1, 0xf9, true),
          (0x61, 1, 0xc2, false),
          (0x70, 1, 0xe0, false),
          (0x98, 1, 0x31, true)
        ),
        5
      )
      // ROR by 1 (D0.1).
      check(
        barrelRotateRight,
        Seq((0x1d, 1, 0x8e, true), (0xc9, 1, 0xe4, true), (0x12, 1, 0x09, false), (0x98, 1, 0x4c, false)),
        5
      )
      // ROL by CL (D2.0).
      check(
        barrelRotateLeft,
        Seq((0x3e, 42, 0xf8, false), (0x61, 13, 0x2c, false), (0x70, 20, 0x07, true), (0x35, 3, 0xa9, true)),
        6
      )
      // ROR by CL (D2.1).
      check(
        barrelRotateRight,
        Seq((0x3e, 42, 0x8f, true), (0x61, 13, 0x0b, false), (0x70, 20, 0x07, false), (0x35, 3, 0xa6, true)),
        6
      )
      // SAR by CL (D2.7), including a count above the width (0x8f by 37).
      check(
        barrelShiftRightArithmetic,
        Seq(
          (0xa1, 22, 0xff, true),
          (0x31, 21, 0x00, false),
          (0x55, 5, 0x02, true),
          (0xb1, 12, 0xff, true),
          (0x8f, 37, 0xff, true)
        ),
        6
      )
      report(bad.toList, count)
    }
  }
}
