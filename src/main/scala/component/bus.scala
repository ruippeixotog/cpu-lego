package component

import component.BuilderAPI.*
import core.*

// Word-level building blocks for the DSL.
//
// Convention: `bus(0)` is the least significant bit, matching
// `util.Implicits.toBoolVec` (`Int#toBoolVec`), which also indexes bit 0 as
// the LSB. All helpers below follow this convention.
//
// Structure is plain `Vector` operations — there are no `slice`/`concat`
// helpers on purpose: use `bus.slice(from, until)` and `a ++ b` directly.

/** A constant `width`-bit bus holding `value` (`bus(0)` is the LSB).
  *
  * Only the lowest `width` bits of `value` are used.
  */
def const(width: Int, value: Long): Bus = {
  assert(width >= 0 && width <= 63, s"const width must be between 0 and 63, got $width")
  (0 until width).map(i => if ((value & (1L << i)) != 0) High else Low).toVector
}

/** Pads `bus` with [[core.Low]] up to `width` bits.
  */
def zeroExtend(bus: Bus, width: Int): Bus = {
  assert(width >= bus.length, s"zeroExtend target width $width is smaller than bus width ${bus.length}")
  bus ++ Vector.fill(width - bus.length)(Low)
}

/** Pads `bus` with copies of its most significant bit up to `width` bits.
  */
def signExtend(bus: Bus, width: Int): Bus = {
  assert(bus.nonEmpty, "signExtend needs a non-empty bus to take the sign bit from")
  assert(width >= bus.length, s"signExtend target width $width is smaller than bus width ${bus.length}")
  bus ++ Vector.fill(width - bus.length)(bus.last)
}

/** Bitwise NOT of every bit. */
def notB(bus: Bus): Spec[Bus] = newSpec {
  bus.map(not)
}

/** Bitwise AND of two buses of equal width. */
def andB(a: Bus, b: Bus): Spec[Bus] = newSpec {
  assert(a.length == b.length, "Buses must have the same width")
  a.zip(b).map(and)
}

/** Bitwise AND of every bit with a single port. */
def andB(bus: Bus, p: Port): Spec[Bus] = newSpec {
  bus.map(and(_, p))
}

/** Bitwise OR of two buses of equal width. */
def orB(a: Bus, b: Bus): Spec[Bus] = newSpec {
  assert(a.length == b.length, "Buses must have the same width")
  a.zip(b).map(or)
}

/** Bitwise OR of every bit with a single port. */
def orB(bus: Bus, p: Port): Spec[Bus] = newSpec {
  bus.map(or(_, p))
}

/** Bitwise XOR of two buses of equal width. */
def xorB(a: Bus, b: Bus): Spec[Bus] = newSpec {
  assert(a.length == b.length, "Buses must have the same width")
  a.zip(b).map(xor)
}

/** Bitwise XOR of every bit with a single port. */
def xorB(bus: Bus, p: Port): Spec[Bus] = newSpec {
  bus.map(xor(_, p))
}

/** OR-reduction: High iff any bit is High (Low for an empty bus). */
def orR(bus: Bus): Spec[Port] = newSpec {
  if (bus.isEmpty) Low else orM(bus*)
}

/** AND-reduction: High iff every bit is High (High for an empty bus). */
def andR(bus: Bus): Spec[Port] = newSpec {
  if (bus.isEmpty) High else andM(bus*)
}

/** XOR-reduction (parity): High iff an odd number of bits is High (Low for an empty bus). */
def xorR(bus: Bus): Spec[Port] = newSpec {
  if (bus.isEmpty) Low else xorM(bus*)
}

/** Equality with a constant: High iff `bus` holds `value`. */
def eqConst(bus: Bus, value: Long): Spec[Port] = newSpec {
  andR(bus.zip(const(bus.length, value)).map(xnor))
}

/** Equality of two buses of equal width: High iff every bit matches. */
def eqB(a: Bus, b: Bus): Spec[Port] = newSpec {
  assert(a.length == b.length, "Buses must have the same width")
  andR(a.zip(b).map(xnor))
}

/** 2-to-1 multiplexer: `sel` Low selects `a`, High selects `b`. */
def mux2(a: Bus, b: Bus, sel: Port): Spec[Bus] = newSpec {
  assert(a.length == b.length, "Buses must have the same width")
  a.zip(b).map { case (x, y) => mux(Vector(x, y), Vector(sel)) }
}

/** Multiplexer over `words`, generalising [[muxN]] to a `Seq` of buses.
  *
  * All words must have the same width and their count must be `2^sel.length`; word `sel` (as an unsigned index) is
  * driven to the output.
  */
def muxWords(words: Seq[Bus], sel: Bus): Spec[Bus] = newSpec {
  val ws = words.toVector
  assert(ws.nonEmpty, "At least one word is required")
  val width = ws.head.length
  assert(ws.forall(_.length == width), "All words must have the same width")
  assert(ws.length == 1 << sel.length, "Number of words must be 2^sel.length")
  if (width == 0) Vector()
  else muxN(ws.flatten, sel, width)
}

/** One-hot multiplexer: output bit `i` is the OR of `words(j)(i)` over all words `j` whose select line is High.
  */
def oneHotMux(words: Seq[Bus], selects: Bus): Spec[Bus] = newSpec {
  val ws = words.toVector
  assert(ws.length == selects.length, "One select line per word is required")
  if (ws.isEmpty) Vector()
  else {
    val width = ws.head.length
    assert(ws.forall(_.length == width), "All words must have the same width")
    (0 until width).map { i =>
      orM(ws.zip(selects).map { case (w, s) => and(w(i), s) }*)
    }.toVector
  }
}

/** Priority encoder: `out` is the binary index of the highest set bit and `valid` is High iff any input bit is High.
  *
  * `out` is `ceil(log2(bus.length))` bits wide (empty for a single input); when no input is High, `out` is zero and
  * `valid` is Low.
  */
def priorityEncoder(bus: Bus): Spec[(Bus, Port)] = newSpec {
  assert(bus.nonEmpty, "Priority encoder needs at least one input")
  val selected = bus.indices.map { i =>
    if (i == bus.length - 1) bus(i)
    else and(bus(i), not(orM(bus.drop(i + 1)*)))
  }.toVector
  val outWidth = 32 - Integer.numberOfLeadingZeros(bus.length - 1)
  val out = (0 until outWidth).map { k =>
    orM(selected.zipWithIndex.collect { case (s, i) if ((i >> k) & 1) == 1 => s }*)
  }.toVector
  (out, orR(bus))
}
