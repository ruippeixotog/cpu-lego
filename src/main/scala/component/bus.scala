package component

import scala.compiletime.ops.int.*

import component.BuilderAPI.*
import core.*

// Word-level building blocks for the DSL.
//
// `bus(0)` is the LSB, as in `util.Implicits.toBoolVec`.
// Slicing and concatenation live on `Bus`: `bus.take(n)`, `bus.drop(n)`, `a ++ b`.

/** `ceil(log2(N))`: the width of a binary index into `N` items. */
type CLog2[N <: Int] = 32 - NumberOfLeadingZeros[N - 1]

/** A constant `width`-bit bus holding `value` (`bus(0)` is the LSB).
  *
  * Only the lowest `width` bits of `value` are used.
  */
def const(width: Int, value: Long): Bus[width.type] = {
  assert(width >= 0 && width <= 63, s"const width must be between 0 and 63, got $width")
  Bus.of(width)((0 until width).map(i => if ((value & (1L << i)) != 0) High else Low).toVector)
}

/** Pads `bus` with [[core.Low]] up to `width` bits.
  */
def zeroExtend[N <: Int](bus: Bus[N], width: Int)(using Width.Le[N, width.type]): Bus[width.type] = {
  assert(width >= bus.length, s"zeroExtend target width $width is smaller than bus width ${bus.length}")
  Bus.of(width)(bus.ports ++ Vector.fill(width - bus.length)(Low))
}

/** Pads `bus` with copies of its most significant bit up to `width` bits.
  */
def signExtend[N <: Int](bus: Bus[N], width: Int)(using Width.Le[N, width.type]): Bus[width.type] = {
  assert(!bus.isEmpty, "signExtend needs a non-empty bus to take the sign bit from")
  assert(width >= bus.length, s"signExtend target width $width is smaller than bus width ${bus.length}")
  Bus.of(width)(bus.ports ++ Vector.fill(width - bus.length)(bus.ports.last))
}

/** Bitwise NOT of every bit. */
def notB[N <: Int](bus: Bus[N]): Spec[Bus[N]] = newSpec {
  bus.map(not)
}

/** Bitwise AND of two buses of equal width. */
def andB[N <: Int](a: Bus[N], b: Bus[N]): Spec[Bus[N]] = newSpec {
  a.zipMap(b)(and)
}

/** Bitwise AND of every bit with a single port. */
def andB[N <: Int](bus: Bus[N], p: Port): Spec[Bus[N]] = newSpec {
  bus.map(and(_, p))
}

/** Bitwise OR of two buses of equal width. */
def orB[N <: Int](a: Bus[N], b: Bus[N]): Spec[Bus[N]] = newSpec {
  a.zipMap(b)(or)
}

/** Bitwise OR of every bit with a single port. */
def orB[N <: Int](bus: Bus[N], p: Port): Spec[Bus[N]] = newSpec {
  bus.map(or(_, p))
}

/** Bitwise XOR of two buses of equal width. */
def xorB[N <: Int](a: Bus[N], b: Bus[N]): Spec[Bus[N]] = newSpec {
  a.zipMap(b)(xor)
}

/** Bitwise XOR of every bit with a single port. */
def xorB[N <: Int](bus: Bus[N], p: Port): Spec[Bus[N]] = newSpec {
  bus.map(xor(_, p))
}

/** OR-reduction: High iff any bit is High (Low for an empty bus). */
def orR[N <: Int](bus: Bus[N]): Spec[Port] = newSpec {
  if (bus.isEmpty) Low else orM(bus.ports*)
}

/** AND-reduction: High iff every bit is High (High for an empty bus). */
def andR[N <: Int](bus: Bus[N]): Spec[Port] = newSpec {
  if (bus.isEmpty) High else andM(bus.ports*)
}

/** XOR-reduction (parity): High iff an odd number of bits is High (Low for an empty bus). */
def xorR[N <: Int](bus: Bus[N]): Spec[Port] = newSpec {
  if (bus.isEmpty) Low else xorM(bus.ports*)
}

/** Equality with a constant: High iff `bus` holds `value`. */
def eqConst[N <: Int](bus: Bus[N], value: Long): Spec[Port] = newSpec {
  assert(value >= 0 && value < (1L << bus.length), s"eqConst value $value does not fit in ${bus.length} bits")
  val bits = bus.ports.zip(const(bus.length, value).ports).map { case (b, c) => if (c == High) b else not(b) }
  andR(Bus.sameAs(bus)(bits))
}

/** Equality of two buses of equal width: High iff every bit matches. */
def eqB[N <: Int](a: Bus[N], b: Bus[N]): Spec[Port] = newSpec {
  andR(a.zipMap(b)(xnor))
}

/** 2-to-1 multiplexer: `sel` Low selects `a`, High selects `b`. */
def mux2[N <: Int](a: Bus[N], b: Bus[N], sel: Port): Spec[Bus[N]] = newSpec {
  a.zipMap(b) { (x, y) => mux(Bus(x, y), Bus(sel)) }
}

/** Multiplexer over `words`, generalising [[muxN]] to a `Seq` of buses.
  *
  * All words must have the same width and their count must be `2^sel.length`; word `sel` (as an unsigned index) is
  * driven to the output.
  */
def muxWords[W <: Int, S <: Int](words: Seq[Bus[W]], sel: Bus[S]): Spec[Bus[W]] = newSpec {
  val ws = words.toVector
  assert(ws.nonEmpty, "At least one word is required")
  val width = ws.head.length
  assert(ws.forall(_.length == width), "All words must have the same width")
  assert(ws.length == 1 << sel.length, "Number of words must be 2^sel.length")
  if (width == 0) ws.head
  else Bus.sameAs(ws.head)(muxN(Bus.assume[(1 << S) * width.type](ws.flatMap(_.ports)), sel, width).ports)
}

/** One-hot multiplexer: output bit `i` is the OR of `words(j)(i)` over all words `j` whose select line is High.
  */
def oneHotMux[W <: Int, K <: Int](words: Seq[Bus[W]], selects: Bus[K]): Spec[Bus[W]] = newSpec {
  val ws = words.toVector
  assert(ws.length == selects.length, "One select line per word is required")
  if (ws.isEmpty) Bus.assume(Vector())
  else {
    val width = ws.head.length
    assert(ws.forall(_.length == width), "All words must have the same width")
    Bus.sameAs(ws.head)((0 until width).map { i =>
      orM(ws.zip(selects.ports).map { case (w, s) => and(w(i), s) }*)
    }.toVector)
  }
}

/** Priority encoder: `out` is the binary index of the highest set bit and `valid` is High iff any input bit is High.
  *
  * `out` is `ceil(log2(bus.length))` bits wide (empty for a single input); when no input is High, `out` is zero and
  * `valid` is Low.
  */
def priorityEncoder[N <: Int](bus: Bus[N]): Spec[(Bus[CLog2[N]], Port)] = newSpec {
  assert(!bus.isEmpty, "Priority encoder needs at least one input")
  val anyFrom = bus.ports.init.scanRight(bus.ports.last)(or) // anyFrom(i) = OR of bus(i) and above
  val selected = bus.ports.indices.map { i =>
    if (i == bus.length - 1) bus(i)
    else and(bus(i), not(anyFrom(i + 1)))
  }.toVector
  val outWidth = 32 - Integer.numberOfLeadingZeros(bus.length - 1)
  val out = (0 until outWidth).map { k =>
    orM(selected.zipWithIndex.collect { case (s, i) if ((i >> k) & 1) == 1 => s }*)
  }.toVector
  (Bus.assume(out), anyFrom(0))
}
