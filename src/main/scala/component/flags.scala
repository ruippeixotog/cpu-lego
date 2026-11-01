package component

import component.BuilderAPI.*
import core.*

// Flag generators shared by the ALU blocks.
//
// C needs no generator: it is the carry out returned by the adders, subtractors
// and shifters themselves. V is covered by the adders' overflow outputs for a
// full operation, and by [[flagVAdd]]/[[flagVSub]] when only the operands and
// the result are at hand (as with the 6502 decimal-mode intermediates).

/** N flag: the most significant bit of `bus`. */
def flagN(bus: Bus): Spec[Port] = newSpec {
  bus.last
}

/** Z flag: High iff every bit of `bus` is Low (High for an empty bus). */
def flagZ(bus: Bus): Spec[Port] = newSpec {
  not(orR(bus))
}

/** P flag (8086 parity): High iff the low 8 bits of `bus` contain an even number of High bits.
  *
  * Bits above the low 8 do not participate; a bus shorter than 8 bits is effectively zero-extended, which does not
  * change the parity.
  */
def flagP(bus: Bus): Spec[Port] = newSpec {
  not(xorR(bus.take(8)))
}

/** V flag for an addition: High iff `a` and `b` have the same sign bit and `sum`'s sign bit differs from it.
  *
  * All three buses must have the same width.
  */
def flagVAdd(a: Bus, b: Bus, sum: Bus): Spec[Port] = newSpec {
  assert(a.length == b.length && b.length == sum.length, "Buses must have the same width")
  and(not(xor(a.last, b.last)), xor(sum.last, a.last))
}

/** V flag for a subtraction: High iff `a` and `b` have different sign bits and `diff`'s sign bit differs from `a`'s.
  *
  * All three buses must have the same width.
  */
def flagVSub(a: Bus, b: Bus, diff: Bus): Spec[Port] = newSpec {
  assert(a.length == b.length && b.length == diff.length, "Buses must have the same width")
  and(xor(a.last, b.last), xor(diff.last, a.last))
}

/** AF flag (8086 auxiliary carry) for an addition: the carry out of bit 3 of `a + b + cin`.
  *
  * `a` and `b` must have the same width and be at least 4 bits wide.
  */
def flagAFAdd(a: Bus, b: Bus, cin: Port): Spec[Port] = newSpec {
  assert(a.length == b.length, "Buses must have the same width")
  assert(a.length >= 4, "AF operands must be at least 4 bits")
  val (_, cout, _) = rippleAdder(a.take(4), b.take(4), cin)
  cout
}

/** AF flag (8086 auxiliary carry) for a subtraction: the borrow out of bit 3 of `a - b - borrow`.
  *
  * `borrow` is the borrow into bit 0: Low for a plain subtraction, the incoming carry flag for an 8086 SBB. `a` and `b`
  * must have the same width and be at least 4 bits wide.
  */
def flagAFSub(a: Bus, b: Bus, borrow: Port): Spec[Port] = newSpec {
  assert(a.length == b.length, "Buses must have the same width")
  assert(a.length >= 4, "AF operands must be at least 4 bits")
  val (_, cout, _) = rippleAdder(a.take(4), b.take(4).map(not), not(borrow))
  not(cout)
}
