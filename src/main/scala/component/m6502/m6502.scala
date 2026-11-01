package component.m6502

import component.*
import component.BuilderAPI.*
import core.*

// One-bit shifts and rotates (6502 ASL/LSR/ROL/ROR) and the NMOS 6502 decimal
// mode (ADC/SBC).
//
// The 6502 blocks implement the NMOS 6502 only. The CMOS 65C02 sets N, V and Z
// from the decimal result and therefore differs; see the scaladoc on
// [[decimalAdc]] for the exact NMOS rules.

/** Arithmetic shift left by one (6502 ASL).
  *
  * Returns `(result, cout)`: `result` is `a` shifted left with Low shifted in, and `cout` is the bit shifted out (`a`'s
  * most significant bit). The carry in is not used.
  */
def asl(a: Bus[8]): Spec[(Bus[8], Port)] = newSpec {
  (Low +: a.take(7), a(7))
}

/** Logical shift right by one (6502 LSR).
  *
  * Returns `(result, cout)`: `result` is `a` shifted right with Low shifted in, and `cout` is the bit shifted out
  * (`a`'s least significant bit).
  */
def lsr(a: Bus[8]): Spec[(Bus[8], Port)] = newSpec {
  (a.drop(1) :+ Low, a(0))
}

/** Rotate left by one through carry (6502 ROL).
  *
  * Returns `(result, cout)`: `result` is `a` rotated left with `cin` shifted in, and `cout` is the bit rotated out
  * (`a`'s most significant bit).
  */
def rol(a: Bus[8], cin: Port): Spec[(Bus[8], Port)] = newSpec {
  (cin +: a.take(7), a(7))
}

/** Rotate right by one through carry (6502 ROR).
  *
  * Returns `(result, cout)`: `result` is `a` rotated right with `cin` shifted in, and `cout` is the bit rotated out
  * (`a`'s least significant bit).
  */
def ror(a: Bus[8], cin: Port): Spec[(Bus[8], Port)] = newSpec {
  (a.drop(1) :+ cin, a(0))
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
  * Sources for the NMOS decimal behaviour:
  *   - Bruce Clark, "Decimal Mode" tutorial, 6502.org — the nibble-pair algorithm and its test program.
  *   - Visual6502 wiki, "6502DecimalMode" (NESdev) — test vectors drawn from Clark's test suite, used as known vectors
  *     in the spec.
  *   - Codebase64 wiki, "Decimal mode in NMOS 6500 series" — the same nibble-pair formulation, including the order in
  *     which the flags are sampled (see above).
  *
  * Returns `(result, carry, overflow, negative, zero)`, in that order.
  */
def decimalAdc(a: Bus[8], b: Bus[8], cin: Port): Spec[(Bus[8], Port, Port, Port, Port)] = newSpec {
  val (lowSum, lowCout, _) = rippleAdder(a.take(4), b.take(4), cin)
  // The nibble sum is 5 bits (lowSum plus lowCout); it exceeds 9 iff it carried out or its low 4 bits exceed 9.
  val lowGt9 = or(lowCout, nibbleGt9(lowSum))
  val (lowSumPlus6, _, _) = rippleAdder(lowSum, const(4, 6), Low)
  val lowRes = mux2(lowSum, lowSumPlus6, lowGt9)
  val (highSum, highCout, _) = rippleAdder(a.drop(4), b.drop(4), lowGt9)
  val highGt9 = or(highCout, nibbleGt9(highSum))
  val (highSumPlus6, _, _) = rippleAdder(highSum, const(4, 6), Low)
  val highRes = mux2(highSum, highSumPlus6, highGt9)
  val (binSum, _, _) = rippleAdder(a, b, cin)
  (
    lowRes ++ highRes,
    highGt9,
    flagVAdd(a, b, lowRes ++ highSum),
    highSum(3),
    flagZ(binSum)
  )
}

/** NMOS 6502 SBC in decimal mode.
  *
  * Each nibble is subtracted with the borrow from the nibble below, and a nibble that borrows is corrected by
  * subtracting 6 from it (modulo 16). Unlike ADC, the flags are exactly the binary subtraction flags: N is bit 7 of the
  * binary difference, V is the binary subtraction overflow, Z is set iff the binary difference is zero, and C is set
  * iff the binary subtraction did not borrow. The decimal mode affects only the result byte.
  *
  * Sources for the NMOS decimal behaviour: see [[decimalAdc]].
  *
  * Returns `(result, carry, overflow, negative, zero)`, in that order, where `carry` is set iff no borrow was needed
  * (the 6502 carry flag after SBC) and `cin` is that flag on entry (set means no borrow in).
  */
def decimalSbc(a: Bus[8], b: Bus[8], cin: Port): Spec[(Bus[8], Port, Port, Port, Port)] = newSpec {
  // Low nibble: `a - b` with the incoming carry.
  val (lowDiff, lowCout, _) = rippleAdder(a.take(4), b.take(4).map(not), cin)
  val lowBorrow = not(lowCout)
  val (lowDiffMinus6, _, _) = subtractor(lowDiff, const(4, 6))
  val lowRes = mux2(lowDiff, lowDiffMinus6, lowBorrow)
  // High nibble: the borrow out of the low nibble is the borrow in here, i.e. the carry in is lowCout.
  val (highDiff, highCout, highOverflow) = rippleAdder(a.drop(4), b.drop(4).map(not), lowCout)
  val highBorrow = not(highCout)
  val (highDiffMinus6, _, _) = subtractor(highDiff, const(4, 6))
  val highRes = mux2(highDiff, highDiffMinus6, highBorrow)
  (lowRes ++ highRes, highCout, highOverflow, highDiff(3), flagZ(lowDiff ++ highDiff))
}
