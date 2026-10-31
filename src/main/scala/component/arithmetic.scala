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
def binaryAdder(ins1: Bus, ins2: Bus, carryIn: Port = Low): Spec[(Bus, Port)] = newSpec {
  val (outs, carry) = ins1.zip(ins2).foldLeft((List.empty[Port], carryIn)) { case ((prevOuts, prevCarry), (a, b)) =>
    val (out, carry) = fullAdder(a, b, prevCarry)
    (out :: prevOuts, carry)
  }
  (outs.reverse.toVector, carry)
}

/** A 2's-complement adder-subtracter of `ins1.length` bits (page 86).
  */
def addSub(ins1: Bus, ins2: Bus, sub: Port): Spec[Bus] = newSpec {
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
def rippleAdder(a: Bus, b: Bus, cin: Port = Low): Spec[(Bus, Port, Port)] = newSpec {
  assert(a.length == b.length, "Buses must have the same width")
  // chain is the carry chain reversed: head is the carry out, element (1) the carry into the MSB.
  val (sumRev, chain) = a.zip(b).foldLeft((Vector.empty[Port], Vector(cin))) { case ((sums, carries), (x, y)) =>
    val (s, c) = fullAdder(x, y, carries.head)
    (s +: sums, c +: carries)
  }
  val cout = chain.head
  val overflow = if (a.isEmpty) Low else xor(chain(1), cout)
  (sumRev.reverse.toVector, cout, overflow)
}

/** A Kogge–Stone parallel-prefix adder of `a.length` bits.
  *
  * Same interface as [[rippleAdder]] — `(sum, cout, overflow)` — with O(log n) gate depth instead of O(n). Computes
  * per-bit generate `g_i = a_i & b_i` and propagate `p_i = a_i ^ b_i`, then combines them with the prefix operator
  * `(G1, P1) ∘ (G2, P2) = (G1 | (P1 & G2), P1 & P2)` (higher group on the left) over `ceil(log2(n))` stages. The carry
  * into bit `i` is `G[i-1:0] | (P[i-1:0] & cin)`.
  */
def prefixAdder(a: Bus, b: Bus, cin: Port = Low): Spec[(Bus, Port, Port)] = newSpec {
  assert(a.length == b.length, "Buses must have the same width")
  val n = a.length
  if (n == 0) {
    (Vector.empty[Port], cin, Low)
  } else {
    val gp: Vector[(Port, Port)] = a.zip(b).map { case (x, y) => (and(x, y), xor(x, y)) }
    val stages = 32 - Integer.numberOfLeadingZeros(n - 1) // ceil(log2(n))
    // After the stages, cell i holds (G[i:0], P[i:0]).
    val prefix = (0 until stages).foldLeft(gp) { (cur, k) =>
      val d = 1 << k
      cur.zipWithIndex.map { case ((g, p), i) =>
        if (i < d) (g, p)
        else {
          val (gl, pl) = cur(i - d)
          (or(g, and(p, gl)), and(p, pl))
        }
      }
    }
    // carries(i) is the carry into bit i.
    val carries: Vector[Port] = cin +: (0 until n - 1).map { i =>
      val (g, p) = prefix(i)
      or(g, and(p, cin))
    }.toVector
    val sum: Bus = gp.zip(carries).map { case ((_, p), c) => xor(p, c) }
    val (gn, pn) = prefix(n - 1)
    val cout = or(gn, and(pn, cin))
    val overflow = xor(carries(n - 1), cout)
    (sum, cout, overflow)
  }
}

/** Increments `a` by one when `inc` is High, passing `a` through otherwise.
  *
  * Returns `(result, cout)`: `cout` is the carry out of the most significant bit.
  */
def incrementer(a: Bus, inc: Port): Spec[(Bus, Port)] = newSpec {
  // a + 0 + inc
  val (sum, cout, _) = rippleAdder(a, Vector.fill(a.length)(Low), inc)
  (sum, cout)
}

/** Decrements `a` by one when `dec` is High, passing `a` through otherwise.
  *
  * Returns `(result, cout)`: `cout` is the raw adder carry — when `dec` is High it is High iff no borrow was needed (`a
  * >= 1`); when `dec` is Low it is always Low.
  */
def decrementer(a: Bus, dec: Port): Spec[(Bus, Port)] = newSpec {
  // a + (dec ? 11..1 : 00..0) + 0 = a - (dec ? 1 : 0)
  val (diff, cout, _) = rippleAdder(a, Vector.fill(a.length)(dec), Low)
  (diff, cout)
}

/** Subtracts `b` from `a` as `a + ~b + 1` (two's complement).
  *
  * Returns `(diff, cout, overflow)`: `cout` is High iff no borrow was needed (`a >= b` as unsigned); `overflow` is the
  * signed overflow of the subtraction.
  */
def subtractor(a: Bus, b: Bus): Spec[(Bus, Port, Port)] = newSpec {
  assert(a.length == b.length, "Buses must have the same width")
  rippleAdder(a, b.map(xor(_, High)), High)
}

/** Compares two buses of equal width.
  *
  * Returns `(eq, ltUnsigned, ltSigned)`, following the flag conventions above: `ltSigned` is `overflow XOR diff.msb`,
  * i.e. the sign of `a - b` corrected for signed overflow.
  */
def compare(a: Bus, b: Bus): Spec[(Port, Port, Port)] = newSpec {
  assert(a.length == b.length, "Buses must have the same width")
  val (diff, cout, overflow) = subtractor(a, b)
  val eq = not(orR(diff))
  val ltUnsigned = not(cout)
  val ltSigned = if (a.isEmpty) Low else xor(overflow, diff.last)
  (eq, ltUnsigned, ltSigned)
}
