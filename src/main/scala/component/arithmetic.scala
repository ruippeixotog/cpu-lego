package component

import component.BuilderAPI.*
import core.*

/** A full adder (page 81).
  */
def fullAdder(in1: Port, in2: Port, in3: Port): Spec[(Port, Port)] = newSpec {
  val carry = orM(and(in1, in2), and(in1, in3), and(in2, in3))
  val out = xorM(in1, in2, in3)
  (out, carry)
}

/** A binary adder of `ins1.length` bits (page 82).
  */
def binaryAdder[N <: Int](ins1: Bus[N], ins2: Bus[N], carryIn: Port = Low): Spec[(Bus[N], Port)] = newSpec {
  ins1.zipMapAccum(ins2)(carryIn) { (carry, a, b) => fullAdder(a, b, carry) }
}

/** A 2's-complement adder-subtracter of `ins1.length` bits (page 86).
  */
def addSub[N <: Int](ins1: Bus[N], ins2: Bus[N], sub: Port): Spec[Bus[N]] = newSpec {
  binaryAdder(ins1, ins2.map(xor(_, sub)), sub)._1
}

/** Flag conventions for the arithmetic blocks below.
  *
  *   - `cout`: the carry out of the most significant bit. For addition, High means the unsigned result did not fit. For
  *     subtraction `a - b`, High means no borrow was needed, i.e. `a >= b` as unsigned.
  *   - `overflow`: signed (two's-complement) overflow — the carry into the MSB XOR the carry out of the MSB.
  *   - `eq` / `ltUnsigned` / `ltSigned` (from [[compare]]): High iff `a == b`, `a < b` as unsigned, `a < b` as
  *     two's-complement signed, respectively.
  */

/** A ripple-carry adder of `a.length` bits.
  *
  * Returns `(sum, cout, overflow)`: `sum` is the `a.length`-bit result, `cout` the carry out of the most significant
  * bit, and `overflow` the signed overflow (carry into the MSB XOR `cout`).
  */
def rippleAdder[N <: Int](a: Bus[N], b: Bus[N], cin: Port = Low): Spec[(Bus[N], Port, Port)] = newSpec {
  // The state is (carry out of the previous bit, carry into the previous bit).
  val (sum, (cout, carryIntoMsb)) = a.zipMapAccum(b)((cin, cin)) { case ((carry, _), x, y) =>
    val (s, c) = fullAdder(x, y, carry)
    (s, (c, carry))
  }
  val overflow = if (a.isEmpty) Low else xor(carryIntoMsb, cout)
  (sum, cout, overflow)
}

/** A Kogge–Stone parallel-prefix adder of `a.length` bits.
  *
  * Same interface as [[rippleAdder]] — `(sum, cout, overflow)` — with O(log n) gate depth instead of O(n). Computes
  * per-bit generate `g_i = a_i & b_i` and propagate `p_i = a_i ^ b_i`, then combines them with the prefix operator
  * `(G1, P1) ∘ (G2, P2) = (G1 | (P1 & G2), P1 & P2)` (higher group on the left) over `ceil(log2(n))` stages. The carry
  * into bit `i` is `G[i-1:0] | (P[i-1:0] & cin)`.
  */
def prefixAdder[N <: Int](a: Bus[N], b: Bus[N], cin: Port = Low): Spec[(Bus[N], Port, Port)] = newSpec {
  val n = a.length
  if (n == 0) {
    (a, cin, Low)
  } else {
    val g0 = a.zipMap(b)(and)
    val p0 = a.zipMap(b)(xor)
    val stages = Width.clog2(n)
    // After the stages, bit i of (gs, ps) holds (G[i:0], P[i:0]).
    val (gs, ps) = (0 until stages).foldLeft((g0, p0)) { case ((g, p), k) =>
      val d = 1 << k
      val nextG = g.mapWithIndex { (gi, i) => if (i < d) gi else or(gi, and(p(i), g(i - d))) }
      val nextP = p.mapWithIndex { (pi, i) => if (i < d) pi else and(pi, p(i - d)) }
      (nextG, nextP)
    }
    // carries(i) is the carry into bit i.
    val carries = a.mapWithIndex { (_, i) => if (i == 0) cin else or(gs(i - 1), and(ps(i - 1), cin)) }
    val sum = p0.zipMap(carries)(xor)
    val cout = or(gs(n - 1), and(ps(n - 1), cin))
    val overflow = xor(carries(n - 1), cout)
    (sum, cout, overflow)
  }
}

/** Increments `a` by one when `inc` is High, passing `a` through otherwise.
  *
  * Returns `(result, cout)`: `cout` is the carry out of the most significant bit.
  */
def incrementer[N <: Int](a: Bus[N], inc: Port): Spec[(Bus[N], Port)] = newSpec {
  // a + 0 + inc
  val (sum, cout, _) = rippleAdder(a, a.map(_ => Low), inc)
  (sum, cout)
}

/** Decrements `a` by one when `dec` is High, passing `a` through otherwise.
  *
  * Returns `(result, cout)`: `cout` is the raw adder carry — when `dec` is High it is High iff no borrow was needed (`a
  * >= 1`); when `dec` is Low it is always Low.
  */
def decrementer[N <: Int](a: Bus[N], dec: Port): Spec[(Bus[N], Port)] = newSpec {
  // a + (dec ? 11..1 : 00..0) + 0 = a - (dec ? 1 : 0)
  val (diff, cout, _) = rippleAdder(a, a.map(_ => dec), Low)
  (diff, cout)
}

/** Subtracts `b` from `a` as `a + ~b + 1` (two's complement).
  *
  * Returns `(diff, cout, overflow)`: `cout` is High iff no borrow was needed (`a >= b` as unsigned); `overflow` is the
  * signed overflow of the subtraction.
  */
def subtractor[N <: Int](a: Bus[N], b: Bus[N]): Spec[(Bus[N], Port, Port)] = newSpec {
  assert(a.length == b.length, "Buses must have the same width")
  rippleAdder(a, b.map(xor(_, High)), High)
}

/** Compares two buses of equal width.
  *
  * Returns `(eq, ltUnsigned, ltSigned)`, following the flag conventions above: `ltSigned` is `overflow XOR diff.msb`,
  * i.e. the sign of `a - b` corrected for signed overflow.
  */
def compare[N <: Int](a: Bus[N], b: Bus[N]): Spec[(Port, Port, Port)] = newSpec {
  assert(a.length == b.length, "Buses must have the same width")
  val (diff, cout, overflow) = subtractor(a, b)
  val eq = not(orR(diff))
  val ltUnsigned = not(cout)
  val ltSigned = if (a.isEmpty) Low else xor(overflow, diff.last)
  (eq, ltUnsigned, ltSigned)
}

/** A nibble is greater than 9 iff its top bit is set and either of the next two bits is set (values 10..15). */
def nibbleGt9(nibble: Bus): Spec[Port] = newSpec {
  assert(nibble.length == 4, "A nibble must be 4 bits")
  and(nibble(3), or(nibble(2), nibble(1)))
}
