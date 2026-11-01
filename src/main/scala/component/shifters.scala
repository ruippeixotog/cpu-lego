package component

import component.BuilderAPI.*
import core.*

// Variable-amount barrel shifters and rotators (8086 SHL/SHR/SAR/ROL/ROR).
//
// Buses are LSB-first (`bus(0)` is the least significant bit), as everywhere in
// this library. The amount is an unsigned bus. Shifting by the width or more
// yields all Low for SHL/SHR and all copies of the sign bit for SAR; for a
// rotate, the amount wraps modulo the width, since rotating by the width is
// the identity. The carry out follows the 8086 rules: the last bit shifted out
// for shifts, and the bit that wrapped into the result for rotates.

/** The carry out of a barrel shift of `a` by `k`, for `1 <= k <= a.length`: for a left shift it is `a(n - k)`, for a
  * right shift it is `a(k - 1)`. Shifting by 0 or by more than the width shifts out Low (once every bit has left, only
  * Low remains), so those cases contribute nothing to the OR.
  */
private def barrelShiftCout[N <: Int, A <: Int](a: Bus[N], amount: Bus[A], left: Boolean): Spec[Port] = newSpec {
  val bits = if (left) a.indices.reverse else a.indices
  val terms = bits.zipWithIndex.collect {
    case (bit, i) if i + 1 < (1L << amount.length) => and(eqConst(amount, i + 1), a(bit))
  }
  orR(Bus.dynamic(terms))
}

/** Applies `shift` by `2^j` to `cur` for each set bit `j` of `amount`. */
private def barrelStages[N <: Int, A <: Int](a: Bus[N], amount: Bus[A], shift: (Bus[N], Int) => Bus[N]): Spec[Bus[N]] =
  newSpec {
    amount.indices.foldLeft(a) { (cur, j) => mux2(cur, shift(cur, 1 << j), amount(j)) }
  }

/** Logical shift left by a variable amount (8086 SHL).
  *
  * Returns `(result, cout)`: `result` is `a` shifted left by `amount` (as an unsigned count), filling with Low, and
  * `cout` is the last bit shifted out — Low when `amount` is 0 or more than the width.
  */
def barrelShiftLeft[N <: Int, A <: Int](a: Bus[N], amount: Bus[A]): Spec[(Bus[N], Port)] = newSpec {
  val shifted = barrelStages(
    a,
    amount,
    _.shiftUp(_, Low)
  )
  (shifted, barrelShiftCout(a, amount, left = true))
}

/** Logical shift right by a variable amount (8086 SHR).
  *
  * Returns `(result, cout)`: `result` is `a` shifted right by `amount` (as an unsigned count), filling with Low, and
  * `cout` is the last bit shifted out — Low when `amount` is 0 or more than the width.
  */
def barrelShiftRight[N <: Int, A <: Int](a: Bus[N], amount: Bus[A]): Spec[(Bus[N], Port)] = newSpec {
  val shifted = barrelStages(
    a,
    amount,
    _.shiftDown(_, Low)
  )
  (shifted, barrelShiftCout(a, amount, left = false))
}

/** Arithmetic shift right by a variable amount (8086 SAR).
  *
  * Returns `(result, cout)`: `result` is `a` shifted right by `amount` (as an unsigned count), filling with copies of
  * `a`'s most significant bit, and `cout` is the last bit shifted out — Low when `amount` is 0, the sign bit when
  * `amount` is at least the width (every further shift shifts out another copy of the sign bit), and `a(k - 1)` for a
  * count `k` below the width.
  */
def barrelShiftRightArithmetic[N <: Int, A <: Int](a: Bus[N], amount: Bus[A]): Spec[(Bus[N], Port)] = newSpec {
  val sign = a.last
  val shifted = barrelStages(
    a,
    amount,
    _.shiftDown(_, sign)
  )
  // `amount` is at least the width iff it is not below it, compared on buses widened to hold the width.
  val width = math.max(amount.length, 32 - Integer.numberOfLeadingZeros(a.length))
  val (_, belowWidth, _) = compare(zeroExtend(amount, width), const(width, a.length))
  val shiftCout = barrelShiftCout(a, amount, left = false)
  (shifted, or(and(not(belowWidth), sign), and(belowWidth, shiftCout)))
}

/** Rotate left by a variable amount (8086 ROL).
  *
  * Returns `(result, cout)`: `result` is `a` rotated left by `amount` modulo the width, and `cout` is the least
  * significant bit of the result (the bit that wrapped into it, and the 8086 carry flag for ROL).
  */
def barrelRotateLeft[N <: Int, A <: Int](a: Bus[N], amount: Bus[A]): Spec[(Bus[N], Port)] = newSpec {
  val rotated = barrelStages(
    a,
    amount,
    _.rotateUp(_)
  )
  (rotated, rotated.head)
}

/** Rotate right by a variable amount (8086 ROR).
  *
  * Returns `(result, cout)`: `result` is `a` rotated right by `amount` modulo the width, and `cout` is the most
  * significant bit of the result (the bit that wrapped into it, and the 8086 carry flag for ROR).
  */
def barrelRotateRight[N <: Int, A <: Int](a: Bus[N], amount: Bus[A]): Spec[(Bus[N], Port)] = newSpec {
  val rotated = barrelStages(
    a,
    amount,
    _.rotateDown(_)
  )
  (rotated, rotated.last)
}
