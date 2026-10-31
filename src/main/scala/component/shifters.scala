package component

import component.BuilderAPI.*
import core.*

// One-bit shifts and rotates, and variable-amount barrel shifters/rotators.
//
// Buses are LSB-first (`bus(0)` is the least significant bit), as everywhere in
// this library. The 1-bit operations use 6502 semantics: ASL/LSR shift in Low
// and ignore the carry in, ROL/ROR rotate through the carry in. The barrel
// operations use 8086 semantics: the amount is an unsigned bus, and shifting by
// the width or more yields all Low (for a rotate, the amount wraps modulo the
// width, since rotating by the width is the identity).

/** Arithmetic shift left by one (6502 ASL).
  *
  * Returns `(result, cout)`: `result` is `a` shifted left with Low shifted in, and `cout` is the bit shifted out (`a`'s
  * most significant bit). The carry in is not used. An empty bus shifts to an empty bus with `cout` Low.
  */
def asl(a: Bus): Spec[(Bus, Port)] = newSpec {
  if (a.isEmpty) (Vector.empty, Low)
  else (Low +: a.dropRight(1), a.last)
}

/** Logical shift right by one (6502 LSR).
  *
  * Returns `(result, cout)`: `result` is `a` shifted right with Low shifted in, and `cout` is the bit shifted out
  * (`a`'s least significant bit). An empty bus shifts to an empty bus with `cout` Low.
  */
def lsr(a: Bus): Spec[(Bus, Port)] = newSpec {
  if (a.isEmpty) (Vector.empty, Low)
  else (a.tail ++ Vector(Low), a.head)
}

/** Rotate left by one through carry (6502 ROL).
  *
  * Returns `(result, cout)`: `result` is `a` rotated left with `cin` shifted in, and `cout` is the bit rotated out
  * (`a`'s most significant bit). An empty bus rotates to an empty bus with `cout` equal to `cin`.
  */
def rol(a: Bus, cin: Port): Spec[(Bus, Port)] = newSpec {
  if (a.isEmpty) (Vector.empty, cin)
  else (cin +: a.dropRight(1), a.last)
}

/** Rotate right by one through carry (6502 ROR).
  *
  * Returns `(result, cout)`: `result` is `a` rotated right with `cin` shifted in, and `cout` is the bit rotated out
  * (`a`'s least significant bit). An empty bus rotates to an empty bus with `cout` equal to `cin`.
  */
def ror(a: Bus, cin: Port): Spec[(Bus, Port)] = newSpec {
  if (a.isEmpty) (Vector.empty, cin)
  else (a.tail ++ Vector(cin), a.head)
}

/** The carry out of a barrel shift of `a` by `k`, for `1 <= k <= a.length`: for a left shift it is `a(n - k)`, for a
  * right shift it is `a(k - 1)`. Shifting by 0 or by more than the width shifts out Low (once every bit has left, only
  * Low remains), so those cases contribute nothing to the OR.
  */
private def barrelShiftCout(a: Bus, amount: Bus, left: Boolean): Spec[Port] = newSpec {
  val terms = (1 to a.length).filter(k => k < (1L << amount.length)).map { k =>
    and(eqConst(amount, k), if (left) a(a.length - k) else a(k - 1))
  }
  orR(terms.toVector)
}

/** A barrel shifter stage: shift `cur` left by `2^j` (filling with Low) when `amount(j)` is High, else pass it through.
  * Shifting by the width or more yields all Low, which is exactly the 8086 behaviour for an oversized count.
  */
private def barrelStages(a: Bus, amount: Bus, shift: (Bus, Int) => Bus): Spec[Bus] = newSpec {
  var cur = a
  for (j <- amount.indices) {
    // 2^j exceeds any realistic bus width once j >= 30; saturate instead of overflowing the shift.
    val shifted = shift(cur, if (j >= 30) Int.MaxValue else 1 << j)
    cur = mux2(cur, shifted, amount(j))
  }
  cur
}

/** Logical shift left by a variable amount (8086 SHL).
  *
  * Returns `(result, cout)`: `result` is `a` shifted left by `amount` (as an unsigned count), filling with Low, and
  * `cout` is the last bit shifted out — Low when `amount` is 0 or at least the width. An empty bus shifts to an empty
  * bus with `cout` Low.
  */
def barrelShiftLeft(a: Bus, amount: Bus): Spec[(Bus, Port)] = newSpec {
  if (a.isEmpty) (Vector.empty, Low)
  else {
    val shifted = barrelStages(
      a,
      amount,
      (cur, s) => if (s >= cur.length) Vector.fill(cur.length)(Low) else Vector.fill(s)(Low) ++ cur.dropRight(s)
    )
    (shifted, barrelShiftCout(a, amount, left = true))
  }
}

/** Logical shift right by a variable amount (8086 SHR).
  *
  * Returns `(result, cout)`: `result` is `a` shifted right by `amount` (as an unsigned count), filling with Low, and
  * `cout` is the last bit shifted out — Low when `amount` is 0 or at least the width. An empty bus shifts to an empty
  * bus with `cout` Low.
  */
def barrelShiftRight(a: Bus, amount: Bus): Spec[(Bus, Port)] = newSpec {
  if (a.isEmpty) (Vector.empty, Low)
  else {
    val shifted = barrelStages(
      a,
      amount,
      (cur, s) => if (s >= cur.length) Vector.fill(cur.length)(Low) else cur.drop(s) ++ Vector.fill(s)(Low)
    )
    (shifted, barrelShiftCout(a, amount, left = false))
  }
}

/** Arithmetic shift right by a variable amount (8086 SAR).
  *
  * Returns `(result, cout)`: `result` is `a` shifted right by `amount` (as an unsigned count), filling with copies of
  * `a`'s most significant bit, and `cout` is the last bit shifted out — Low when `amount` is 0 or at least the width.
  * An empty bus shifts to an empty bus with `cout` Low.
  */
def barrelShiftRightArithmetic(a: Bus, amount: Bus): Spec[(Bus, Port)] = newSpec {
  if (a.isEmpty) (Vector.empty, Low)
  else {
    val sign = a.last
    val shifted = barrelStages(
      a,
      amount,
      (cur, s) => if (s >= cur.length) Vector.fill(cur.length)(sign) else cur.drop(s) ++ Vector.fill(s)(sign)
    )
    (shifted, barrelShiftCout(a, amount, left = false))
  }
}

/** Rotate left by a variable amount (8086 ROL).
  *
  * Returns `(result, cout)`: `result` is `a` rotated left by `amount` modulo the width, and `cout` is the most
  * significant bit of the result (the 8086 carry flag for ROL). An empty bus rotates to an empty bus with `cout` Low.
  */
def barrelRotateLeft(a: Bus, amount: Bus): Spec[(Bus, Port)] = newSpec {
  if (a.isEmpty) (Vector.empty, Low)
  else {
    val n = a.length
    val rotated = barrelStages(
      a,
      amount,
      (cur, s) => {
        val r = s % n
        if (r == 0) cur else cur.slice(n - r, n) ++ cur.slice(0, n - r)
      }
    )
    (rotated, rotated.last)
  }
}

/** Rotate right by a variable amount (8086 ROR).
  *
  * Returns `(result, cout)`: `result` is `a` rotated right by `amount` modulo the width, and `cout` is the least
  * significant bit of the result (the 8086 carry flag for ROR). An empty bus rotates to an empty bus with `cout` Low.
  */
def barrelRotateRight(a: Bus, amount: Bus): Spec[(Bus, Port)] = newSpec {
  if (a.isEmpty) (Vector.empty, Low)
  else {
    val n = a.length
    val rotated = barrelStages(
      a,
      amount,
      (cur, s) => {
        val r = s % n
        if (r == 0) cur else cur.slice(r, n) ++ cur.slice(0, r)
      }
    )
    (rotated, rotated.head)
  }
}
