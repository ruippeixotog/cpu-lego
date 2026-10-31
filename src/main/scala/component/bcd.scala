package component

import component.BuilderAPI.*
import core.*

// BCD arithmetic: the 6502 decimal mode and the 8086 decimal-adjust helpers.
//
// The 6502 blocks implement the NMOS 6502 only. The CMOS 65C02 sets N, V and Z
// from the decimal result and therefore differs; see the scaladoc on
// [[decimalAdc]] for the exact NMOS rules.
//
// Sources for the NMOS decimal behaviour:
//   - Bruce Clark, "Decimal Mode" tutorial, 6502.org — the nibble-pair algorithm
//     and its test program (the reference named by the issue).
//   - Visual6502 wiki, "6502DecimalMode" (NESdev) — test vectors drawn from
//     Clark's test suite, used as known vectors in the spec.
//   - Codebase64 wiki, "Decimal mode in NMOS 6500 series" — the same
//     nibble-pair formulation, including the order in which the flags are
//     sampled (see [[decimalAdc]]).
//
// The 8086 helpers follow the Intel pseudocode for DAA, DAS, AAA and AAS: the
// low-nibble test runs first and its result feeds the high-nibble test.

/** A nibble is greater than 9 iff its top bit is set and either of the next two bits is set (values 10..15). */
private def nibbleGt9(nibble: Bus): Spec[Port] = newSpec {
  assert(nibble.length == 4, "A nibble must be 4 bits")
  and(nibble(3), or(nibble(2), nibble(1)))
}

/** A byte is greater than 0x9F iff its top bit is set and either of the next two bits is set (values 0xA0..0xFF). */
private def byteGt9F(byte: Bus): Spec[Port] = newSpec {
  assert(byte.length == 8, "A byte must be 8 bits")
  and(byte(7), or(byte(6), byte(5)))
}

/** NMOS 6502 ADC in decimal mode.
  *
  * The operands are added as a pair of nibbles, not as a byte with a post-adjustment: the low nibble sum is corrected
  * first, and the carry into the high nibble is the carry out of the *corrected* low nibble — equivalently, the low sum
  * exceeds 9. The high nibble sum is then corrected the same way, and the carry out is set iff the high sum exceeds 9
  * (the corrected high sum then exceeds 15).
  *
  * The flags follow the NMOS hardware, sampled in this order:
  *   - C is the only flag set from the fully corrected result (high sum > 9).
  *   - N is bit 3 of the high nibble sum — after the low correction, before the high correction. It is therefore not in
  *     general bit 7 of the result.
  *   - V uses binary-overflow logic with that same bit in place of the result's bit 7: set iff `a` and `b` have the
  *     same sign bit and bit 3 of the high nibble sum differs from it.
  *   - Z is set from the *binary* sum `a + b + cin`, not from the decimal result: e.g. 0x89 + 0x76 + carry gives
  *     decimal 0x66 while the binary sum wraps to 0x00, so Z is set; conversely 0x99 + 0x01 with no carry gives decimal
  *     0x00 with carry while the binary sum is 0x9A, so Z is clear.
  *
  * Returns `(result, carry, overflow, negative, zero)`, in that order. Both operands must be 8 bits.
  */
def decimalAdc(a: Bus, b: Bus, cin: Port): Spec[(Bus, Port, Port, Port, Port)] = newSpec {
  assert(a.length == 8 && b.length == 8, "Decimal ADC operands must be 8 bits")
  val (lowSum, lowCout, _) = rippleAdder(a.take(4), b.take(4), cin)
  // The nibble sum is 5 bits (lowSum plus lowCout); it exceeds 9 iff it carried out or its low 4 bits exceed 9.
  val lowGt9 = or(lowCout, nibbleGt9(lowSum))
  val (lowSumPlus6, _, _) = rippleAdder(lowSum, const(4, 6), Low)
  val lowRes = mux2(lowSum, lowSumPlus6, lowGt9)
  // The carry into the high nibble is the carry out of the corrected low nibble, which is exactly lowGt9: a low sum
  // of 10..15 corrects to 16..21 (carry out), and a low sum of 16..31 already carried out of the nibble adder.
  val (highSum, highCout, _) = rippleAdder(a.drop(4), b.drop(4), lowGt9)
  val highGt9 = or(highCout, nibbleGt9(highSum))
  val (highSumPlus6, _, _) = rippleAdder(highSum, const(4, 6), Low)
  val highRes = mux2(highSum, highSumPlus6, highGt9)
  val (binSum, _, _) = rippleAdder(a, b, cin)
  (
    lowRes ++ highRes,
    highGt9,
    and(xor(highSum(3), a(7)), not(xor(a(7), b(7)))),
    highSum(3),
    not(orR(binSum))
  )
}

/** NMOS 6502 SBC in decimal mode.
  *
  * Each nibble is subtracted with the borrow from the nibble below, and a nibble that borrows is corrected by
  * subtracting 6 from it (modulo 16). Unlike ADC, the flags are exactly the binary subtraction flags: N is bit 7 of the
  * binary difference, V is the binary subtraction overflow, Z is set iff the binary difference is zero, and C is set
  * iff the binary subtraction did not borrow. The decimal mode affects only the result byte.
  *
  * Returns `(result, carry, overflow, negative, zero)`, in that order, where `carry` is set iff no borrow was needed
  * (the 6502 carry flag after SBC) and `cin` is that flag on entry (set means no borrow in). Both operands must be 8
  * bits.
  */
def decimalSbc(a: Bus, b: Bus, cin: Port): Spec[(Bus, Port, Port, Port, Port)] = newSpec {
  assert(a.length == 8 && b.length == 8, "Decimal SBC operands must be 8 bits")
  // Binary subtraction a - b - (1 - cin), as a + ~b + cin: its flags are the SBC flags.
  val (binDiff, binCout, binOverflow) = rippleAdder(a, b.map(not), cin)
  // Low nibble: the same subtraction on the nibbles, with the same carry in.
  val (lowDiff, lowCout, _) = rippleAdder(a.take(4), b.take(4).map(not), cin)
  val lowBorrow = not(lowCout)
  val (lowDiffMinus6, _, _) = subtractor(lowDiff, const(4, 6))
  val lowRes = mux2(lowDiff, lowDiffMinus6, lowBorrow)
  // High nibble: the borrow out of the low nibble is the borrow in here, i.e. the carry in is lowCout.
  val (highDiff, highCout, _) = rippleAdder(a.drop(4), b.drop(4).map(not), lowCout)
  val highBorrow = not(highCout)
  val (highDiffMinus6, _, _) = subtractor(highDiff, const(4, 6))
  val highRes = mux2(highDiff, highDiffMinus6, highBorrow)
  (lowRes ++ highRes, highCout, binOverflow, binDiff(7), not(orR(binDiff)))
}

/** 8086 DAA (decimal adjust AL after addition).
  *
  * Takes the AL byte after an addition, with the AF and CF produced by that addition. If the low nibble exceeds 9 or AF
  * is set, 6 is added to AL and AF is set, else AF is cleared; then, using the adjusted AL, if it exceeds 0x9F or CF is
  * set, 0x60 is added to AL and CF is set, else CF is cleared.
  *
  * Returns `(result, cf, af)`, in that order. SF, ZF and PF for the result follow from [[flagN]], [[flagZ]] and
  * [[flagP]] and are not returned here.
  */
def daa(al: Bus, af: Port, cf: Port): Spec[(Bus, Port, Port)] = newSpec {
  assert(al.length == 8, "DAA operates on an 8-bit AL")
  val lowAdj = or(nibbleGt9(al.take(4)), af)
  val (alPlus6, _, _) = rippleAdder(al, const(8, 6), Low)
  val alLow = mux2(al, alPlus6, lowAdj)
  val highAdj = or(byteGt9F(alLow), cf)
  val (alPlus60, _, _) = rippleAdder(alLow, const(8, 0x60), Low)
  (mux2(alLow, alPlus60, highAdj), highAdj, lowAdj)
}

/** 8086 DAS (decimal adjust AL after subtraction).
  *
  * The subtraction mirror of [[daa]]: if the low nibble exceeds 9 or AF is set, 6 is subtracted from AL and AF is set,
  * else AF is cleared; then, using the adjusted AL, if it exceeds 0x9F or CF is set, 0x60 is subtracted from AL and CF
  * is set, else CF is cleared.
  *
  * Returns `(result, cf, af)`, in that order.
  */
def das(al: Bus, af: Port, cf: Port): Spec[(Bus, Port, Port)] = newSpec {
  assert(al.length == 8, "DAS operates on an 8-bit AL")
  val lowAdj = or(nibbleGt9(al.take(4)), af)
  val (alMinus6, _, _) = subtractor(al, const(8, 6))
  val alLow = mux2(al, alMinus6, lowAdj)
  val highAdj = or(byteGt9F(alLow), cf)
  val (alMinus60, _, _) = subtractor(alLow, const(8, 0x60))
  (mux2(alLow, alMinus60, highAdj), highAdj, lowAdj)
}

/** 8086 AAA (ASCII adjust AL after addition).
  *
  * If the low nibble of AL exceeds 9 or AF is set, AL is increased by 6, AH is incremented, and AF and CF are set; else
  * AF and CF are cleared. In both cases the high nibble of AL is cleared. The incoming CF is not an input to AAA (the
  * instruction sets CF equal to AF), so it takes only `(al, ah, af)`.
  *
  * Returns `(al, ah, af, cf)`, in that order.
  */
def aaa(al: Bus, ah: Bus, af: Port): Spec[(Bus, Bus, Port, Port)] = newSpec {
  assert(al.length == 8 && ah.length == 8, "AAA operates on 8-bit AL and AH")
  val adj = or(nibbleGt9(al.take(4)), af)
  val (alPlus6, _, _) = rippleAdder(al, const(8, 6), Low)
  val alAdj = mux2(al, alPlus6, adj)
  val (ahPlus1, _) = incrementer(ah, adj)
  (alAdj.take(4) ++ Vector.fill(4)(Low), ahPlus1, adj, adj)
}

/** 8086 AAS (ASCII adjust AL after subtraction).
  *
  * The subtraction mirror of [[aaa]]: if the low nibble of AL exceeds 9 or AF is set, AL is decreased by 6, AH is
  * decremented, and AF and CF are set; else AF and CF are cleared. In both cases the high nibble of AL is cleared. The
  * incoming CF is not an input to AAS.
  *
  * Returns `(al, ah, af, cf)`, in that order.
  */
def aas(al: Bus, ah: Bus, af: Port): Spec[(Bus, Bus, Port, Port)] = newSpec {
  assert(al.length == 8 && ah.length == 8, "AAS operates on 8-bit AL and AH")
  val adj = or(nibbleGt9(al.take(4)), af)
  val (alMinus6, _, _) = subtractor(al, const(8, 6))
  val alAdj = mux2(al, alMinus6, adj)
  val (ahMinus1, _) = decrementer(ah, adj)
  (alAdj.take(4) ++ Vector.fill(4)(Low), ahMinus1, adj, adj)
}
