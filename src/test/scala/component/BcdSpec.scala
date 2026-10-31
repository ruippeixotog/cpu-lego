package component

import scala.collection.mutable.ListBuffer

import component.BuilderAPI.*
import core.*
import org.specs2.execute.Result
import org.specs2.mutable.Specification
import simulator.GateProcessor
import testkit.*
import util.Implicits.*

class BcdSpec extends BaseSpec {

  // --- Scala reference models, implemented from the cited sources ---
  //
  // decimalAdcRef follows Bruce Clark's "Decimal Mode" tutorial (6502.org) as
  // formulated in the Codebase64 wiki's NMOS decimal-mode page: the operands
  // are added as a pair of nibbles, the low nibble is corrected first, and the
  // flags are sampled from the intermediate sums (see the scaladoc on
  // decimalAdc). decimalSbcRef follows the same sources: the result is
  // nibble-corrected, but the flags are the binary subtraction flags.

  /** (result, carry, overflow, negative, zero) for NMOS decimal ADC. */
  private def decimalAdcRef(a: Int, b: Int, cin: Int): (Int, Boolean, Boolean, Boolean, Boolean) = {
    val lowSum = (a & 0xf) + (b & 0xf) + cin
    val lowAdj = if (lowSum > 9) lowSum + 6 else lowSum
    val highSum = (a >> 4) + (b >> 4) + (if (lowAdj > 15) 1 else 0)
    val highAdj = if (highSum > 9) highSum + 6 else highSum
    val result = ((highAdj & 0xf) << 4) | (lowAdj & 0xf)
    val carry = highAdj > 15
    val negative = (highSum & 8) != 0
    val overflow = ((highSum & 8) != 0) != ((a & 0x80) != 0) && ((a & 0x80) == (b & 0x80))
    val zero = ((a + b + cin) & 0xff) == 0
    (result, carry, overflow, negative, zero)
  }

  /** (result, carry, overflow, negative, zero) for NMOS decimal SBC. `cin` is the 6502 carry flag on entry. */
  private def decimalSbcRef(a: Int, b: Int, cin: Int): (Int, Boolean, Boolean, Boolean, Boolean) = {
    val borrowIn = 1 - cin
    val lowRaw = (a & 0xf) - (b & 0xf) - borrowIn
    val lowBorrow = lowRaw < 0
    val lowRes = if (lowBorrow) ((lowRaw & 0xf) - 6) & 0xf else lowRaw & 0xf
    val highRaw = (a >> 4) - (b >> 4) - (if (lowBorrow) 1 else 0)
    val highBorrow = highRaw < 0
    val highRes = if (highBorrow) ((highRaw & 0xf) - 6) & 0xf else highRaw & 0xf
    val result = (highRes << 4) | lowRes
    val carry = !highBorrow
    val binDiff = (a - b - borrowIn) & 0xff
    val negative = (binDiff & 0x80) != 0
    val zero = binDiff == 0
    val overflow = ((a & 0x80) != (b & 0x80)) && ((binDiff & 0x80) != (a & 0x80))
    (result, carry, overflow, negative, zero)
  }

  /** (result, cf, af) for 8086 DAA, from the Intel pseudocode. */
  private def daaRef(al: Int, af: Boolean, cf: Boolean): (Int, Boolean, Boolean) = {
    var a = al
    var afOut = false
    if (((a & 0xf) > 9) || af) { a = (a + 6) & 0xff; afOut = true }
    var cfOut = false
    if (a > 0x9f || cf) { a = (a + 0x60) & 0xff; cfOut = true }
    (a, cfOut, afOut)
  }

  /** (result, cf, af) for 8086 DAS, from the Intel pseudocode. */
  private def dasRef(al: Int, af: Boolean, cf: Boolean): (Int, Boolean, Boolean) = {
    var a = al
    var afOut = false
    if (((a & 0xf) > 9) || af) { a = (a - 6) & 0xff; afOut = true }
    var cfOut = false
    if (a > 0x9f || cf) { a = (a - 0x60) & 0xff; cfOut = true }
    (a, cfOut, afOut)
  }

  /** (al, ah, af, cf) for 8086 AAA, from the Intel pseudocode. */
  private def aaaRef(al: Int, ah: Int, af: Boolean): (Int, Int, Boolean, Boolean) = {
    val adj = ((al & 0xf) > 9) || af
    val alOut = (if (adj) (al + 6) & 0xff else al) & 0x0f
    val ahOut = (if (adj) ah + 1 else ah) & 0xff
    (alOut, ahOut, adj, adj)
  }

  /** (al, ah, af, cf) for 8086 AAS, from the Intel pseudocode. */
  private def aasRef(al: Int, ah: Int, af: Boolean): (Int, Int, Boolean, Boolean) = {
    val adj = ((al & 0xf) > 9) || af
    val alOut = (if (adj) (al - 6) & 0xff else al) & 0x0f
    val ahOut = (if (adj) ah - 1 else ah) & 0xff
    (alOut, ahOut, adj, adj)
  }

  private def report(bad: List[String], count: Int): Result =
    if (bad.nonEmpty) failure(s"$count vectors, mismatches: ${bad.mkString("; ")}")
    else ok

  private def checkBus(
      sim: GateProcessor,
      got: Bus,
      expected: Int,
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

  private def checkDecimal(
      sim: GateProcessor,
      outs: (Bus, Port, Port, Port, Port),
      expected: (Int, Boolean, Boolean, Boolean, Boolean),
      label: String,
      bad: ListBuffer[String]
  ): Unit = {
    val (result, carry, overflow, negative, zero) = outs
    val (expResult, expCarry, expOverflow, expNegative, expZero) = expected
    checkBus(sim, result, expResult, s"$label result", bad)
    checkPort(sim, carry, expCarry, s"$label C", bad)
    checkPort(sim, overflow, expOverflow, s"$label V", bad)
    checkPort(sim, negative, expNegative, s"$label N", bad)
    checkPort(sim, zero, expZero, s"$label Z", bad)
  }

  // Known NMOS vectors from the Visual6502 wiki's decimal-mode page (drawn
  // from Bruce Clark's test suite): (a, b, cin, result, N, V, Z, C).
  private val adcVectors: Seq[(Int, Int, Int, Int, Boolean, Boolean, Boolean, Boolean)] = Seq(
    (0x00, 0x00, 0, 0x00, false, false, true, false),
    (0x79, 0x00, 1, 0x80, true, true, false, false),
    (0x24, 0x56, 0, 0x80, true, true, false, false),
    (0x93, 0x82, 0, 0x75, false, true, false, true),
    (0x89, 0x76, 0, 0x65, false, false, false, true),
    (0x89, 0x76, 1, 0x66, false, false, true, true),
    (0x80, 0xf0, 0, 0xd0, false, true, false, true),
    (0x80, 0xfa, 0, 0xe0, true, false, false, true),
    (0x2f, 0x4f, 0, 0x74, false, false, false, false),
    (0x6f, 0x00, 1, 0x76, false, false, false, false)
  )

  private val sbcVectors: Seq[(Int, Int, Int, Int, Boolean, Boolean, Boolean, Boolean)] = Seq(
    (0x00, 0x00, 0, 0x99, true, false, false, false),
    (0x00, 0x00, 1, 0x00, false, false, true, true),
    (0x00, 0x01, 1, 0x99, true, false, false, false),
    (0x0a, 0x00, 1, 0x0a, false, false, false, true),
    (0x0b, 0x00, 0, 0x0a, false, false, false, true),
    (0x9a, 0x00, 1, 0x9a, true, false, false, true),
    (0x9b, 0x00, 0, 0x9a, true, false, false, true)
  )

  "The decimal reference models" should {
    "reproduce the published NMOS vectors" in {
      val bad = ListBuffer.empty[String]
      for ((a, b, c, r, n, v, z, cf) <- adcVectors) {
        val got = decimalAdcRef(a, b, c)
        if (got != (r, cf, v, n, z)) bad += s"ADC $a $b $c: $got != ($r, $cf, $v, $n, $z)"
      }
      for ((a, b, c, r, n, v, z, cf) <- sbcVectors) {
        val got = decimalSbcRef(a, b, c)
        if (got != (r, cf, v, n, z)) bad += s"SBC $a $b $c: $got != ($r, $cf, $v, $n, $z)"
      }
      report(bad.toList, adcVectors.size + sbcVectors.size)
    }
  }

  "A decimalAdc" should {
    "match the published NMOS vectors" in {
      val aIns = Vector.fill(8)(new Port)
      val bIns = Vector.fill(8)(new Port)
      val cin = new Port
      val (outs, comp) = buildComponent { decimalAdc(aIns, bIns, cin) }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      for ((a, b, c, r, n, v, z, cf) <- adcVectors) {
        sim = sim.set(aIns, a.toBoolVec(8)).set(bIns, b.toBoolVec(8)).set(cin, c == 1).run()
        checkDecimal(sim, outs, (r, cf, v, n, z), s"ADC a=$a b=$b cin=$c", bad)
      }
      report(bad.toList, adcVectors.size)
    }

    "match the reference model on all 8-bit operand pairs and carry-ins" in {
      val aIns = Vector.fill(8)(new Port)
      val bIns = Vector.fill(8)(new Port)
      val cin = new Port
      val (outs, comp) = buildComponent { decimalAdc(aIns, bIns, cin) }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      var count = 0
      for (a <- 0 until 256; b <- 0 until 256; c <- 0 to 1) {
        count += 1
        sim = sim.set(aIns, a.toBoolVec(8)).set(bIns, b.toBoolVec(8)).set(cin, c == 1).run()
        checkDecimal(sim, outs, decimalAdcRef(a, b, c), s"ADC a=$a b=$b cin=$c", bad)
      }
      report(bad.toList, count)
    }
  }

  "A decimalSbc" should {
    "match the published NMOS vectors" in {
      val aIns = Vector.fill(8)(new Port)
      val bIns = Vector.fill(8)(new Port)
      val cin = new Port
      val (outs, comp) = buildComponent { decimalSbc(aIns, bIns, cin) }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      for ((a, b, c, r, n, v, z, cf) <- sbcVectors) {
        sim = sim.set(aIns, a.toBoolVec(8)).set(bIns, b.toBoolVec(8)).set(cin, c == 1).run()
        checkDecimal(sim, outs, (r, cf, v, n, z), s"SBC a=$a b=$b cin=$c", bad)
      }
      report(bad.toList, sbcVectors.size)
    }

    "match the reference model on all 8-bit operand pairs and carry-ins" in {
      val aIns = Vector.fill(8)(new Port)
      val bIns = Vector.fill(8)(new Port)
      val cin = new Port
      val (outs, comp) = buildComponent { decimalSbc(aIns, bIns, cin) }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      var count = 0
      for (a <- 0 until 256; b <- 0 until 256; c <- 0 to 1) {
        count += 1
        sim = sim.set(aIns, a.toBoolVec(8)).set(bIns, b.toBoolVec(8)).set(cin, c == 1).run()
        checkDecimal(sim, outs, decimalSbcRef(a, b, c), s"SBC a=$a b=$b cin=$c", bad)
      }
      report(bad.toList, count)
    }
  }

  "A daa" should {
    "match the Intel pseudocode for all AL values and AF/CF combinations" in {
      val alIns = Vector.fill(8)(new Port)
      val af = new Port
      val cf = new Port
      val ((result, cfOut, afOut), comp) = buildComponent { daa(alIns, af, cf) }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      for (al <- 0 until 256; a <- 0 to 1; c <- 0 to 1) {
        sim = sim.set(alIns, al.toBoolVec(8)).set(af, a == 1).set(cf, c == 1).run()
        val (expResult, expCf, expAf) = daaRef(al, a == 1, c == 1)
        checkBus(sim, result, expResult, s"DAA al=$al af=$a cf=$c", bad)
        checkPort(sim, cfOut, expCf, s"DAA al=$al af=$a cf=$c CF", bad)
        checkPort(sim, afOut, expAf, s"DAA al=$al af=$a cf=$c AF", bad)
      }
      report(bad.toList, 1024)
    }
  }

  "A das" should {
    "match the Intel pseudocode for all AL values and AF/CF combinations" in {
      val alIns = Vector.fill(8)(new Port)
      val af = new Port
      val cf = new Port
      val ((result, cfOut, afOut), comp) = buildComponent { das(alIns, af, cf) }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      for (al <- 0 until 256; a <- 0 to 1; c <- 0 to 1) {
        sim = sim.set(alIns, al.toBoolVec(8)).set(af, a == 1).set(cf, c == 1).run()
        val (expResult, expCf, expAf) = dasRef(al, a == 1, c == 1)
        checkBus(sim, result, expResult, s"DAS al=$al af=$a cf=$c", bad)
        checkPort(sim, cfOut, expCf, s"DAS al=$al af=$a cf=$c CF", bad)
        checkPort(sim, afOut, expAf, s"DAS al=$al af=$a cf=$c AF", bad)
      }
      report(bad.toList, 1024)
    }
  }

  private def checkAaaAas(
      op: (Bus, Bus, Port) => Spec[(Bus, Bus, Port, Port)],
      ref: (Int, Int, Boolean) => (Int, Int, Boolean, Boolean),
      name: String
  ): Result = {
    val alIns = Vector.fill(8)(new Port)
    val ahIns = Vector.fill(8)(new Port)
    val af = new Port
    val ((alOut, ahOut, afOut, cfOut), comp) = buildComponent { op(alIns, ahIns, af) }
    var sim = GateProcessor.setup(comp)
    val bad = ListBuffer.empty[String]
    var count = 0
    // AH only enters through a +/-1, so its edge values (wraparound at 0 and 255) cover it exhaustively in effect.
    for (al <- 0 until 256; ah <- Seq(0, 1, 0x7f, 0x80, 0xfe, 0xff); a <- 0 to 1) {
      count += 1
      sim = sim.set(alIns, al.toBoolVec(8)).set(ahIns, ah.toBoolVec(8)).set(af, a == 1).run()
      val (expAl, expAh, expAf, expCf) = ref(al, ah, a == 1)
      checkBus(sim, alOut, expAl, s"$name al=$al ah=$ah af=$a AL", bad)
      checkBus(sim, ahOut, expAh, s"$name al=$al ah=$ah af=$a AH", bad)
      checkPort(sim, afOut, expAf, s"$name al=$al ah=$ah af=$a AF", bad)
      checkPort(sim, cfOut, expCf, s"$name al=$al ah=$ah af=$a CF", bad)
      if (bad.size >= 5) return report(bad.toList, count)
    }
    val rng = new scala.util.Random(34)
    for (_ <- 0 until 500) {
      count += 1
      val al = rng.nextInt(256)
      val ah = rng.nextInt(256)
      val a = rng.nextInt(2) == 1
      sim = sim.set(alIns, al.toBoolVec(8)).set(ahIns, ah.toBoolVec(8)).set(af, a).run()
      val (expAl, expAh, expAf, expCf) = ref(al, ah, a)
      checkBus(sim, alOut, expAl, s"$name al=$al ah=$ah af=$a AL", bad)
      checkBus(sim, ahOut, expAh, s"$name al=$al ah=$ah af=$a AH", bad)
      checkPort(sim, afOut, expAf, s"$name al=$al ah=$ah af=$a AF", bad)
      checkPort(sim, cfOut, expCf, s"$name al=$al ah=$ah af=$a CF", bad)
      if (bad.size >= 5) return report(bad.toList, count)
    }
    report(bad.toList, count)
  }

  "An aaa" should {
    "match the Intel pseudocode" in {
      checkAaaAas(aaa, aaaRef, "AAA")
    }
  }

  "An aas" should {
    "match the Intel pseudocode" in {
      checkAaaAas(aas, aasRef, "AAS")
    }
  }
}
