package component

import component.BuilderAPI.*
import core.*

/** Textbook active-low SR latch: two NAND gates wired in a loop (page 92).
  *
  * This is the raw memory cell everything else is built from. It is not safe to use directly: driving both inputs Low
  * is forbidden (both outputs go High, and releasing them together can leave the latch oscillating instead of
  * settling). Its inputs must also change one at a time.
  */
private[component] def nandLatch(set: Port, reset: Port): Spec[(Port, Port)] = newSpec {
  val aux1, aux2 = newPort()
  val q = nand(set, aux1)
  val nq = nand(reset, aux2)
  q ~> aux2
  nq ~> aux1
  (q, nq)
}

/** Textbook active-high SR latch: two NOR gates wired in a loop (the NORs are themselves built from NANDs).
  *
  * Same warning as [[nandLatch]]: driving both inputs High is forbidden.
  */
def norLatch(set: Port, reset: Port): Spec[(Port, Port)] = newSpec {
  val aux1, aux2 = newPort()
  val q = nor(reset, aux1)
  val nq = nor(set, aux2)
  q ~> aux2
  nq ~> aux1
  (q, nq)
}

/** A clocked SR latch with asynchronous, active-low `clear` and `preset` (pages 94, 97).
  *
  * Follows `set`/`reset` while `clk` is High and holds while Low. `clear` forces Low, `preset` forces High; both Low
  * holds the current value.
  *
  * Contract: `set` and `reset` must not both be High while `clk` is High, and `clk` must not fall within a few gate
  * delays of a `set`/`reset` change. A violation leaves the inner NAND latch oscillating, so `GateProcessor.run()`
  * never returns.
  */
private[component] def latchClocked(
    set: Port,
    reset: Port,
    clk: Port,
    clear: Port = High,
    preset: Port = High
): Spec[(Port, Port)] =
  newSpec {
    nandLatch(
      or(not(clear), and(preset, nand(set, clk))),
      or(not(preset), and(clear, nand(reset, clk)))
    )
  }

/** Splits the clock into two phase signals for master-slave flip-flops.
  *
  * Produces `(clkM, clkS)`: `clkM` is High while `clk` is Low (the master latch may read its input), `clkS` is High
  * while `clk` is High (the slave latch may read the master). Each phase turns on a little later than the other turns
  * off, so the master is fully closed before the slave opens, and vice versa. The two latches are therefore never open
  * at the same time — without this, input data could shoot straight through both latches within a single clock phase
  * and the flip-flop would stop behaving as edge-triggered.
  *
  * The "one closes before the other opens" ordering holds for any positive gate/wire delays: the turn-on path passes
  * through strictly more gates than the turn-off path, so it is strictly slower.
  */
private[component] def nonOverlapClock(clk: Port): Spec[(Port, Port)] = newSpec {
  val clkM = and(not(clk), not(not(not(clk))))
  val clkS = and(clk, not(not(clk)))
  (clkM, clkS)
}

/** A flip-flop that captures `in` on the rising edge of `clk` (page 96).
  *
  * Two [[latchClocked]]s in series (master and slave), driven by the two phases of [[nonOverlapClock]]. The master
  * reads `in` while the clock is Low; on the rising edge the master closes and the slave opens, copying the master to
  * the output. Because the phases never overlap, the output only changes on the rising edge.
  *
  * The active-low `clear`/`preset` override at any time (asserting both Low holds the current value); they reach both
  * latches, so no stale value survives in the master.
  *
  * Timing contract: keep `in` stable around the rising clock edge; keep the clock slow enough for all signals to settle
  * between edges; do not toggle `clear` and `preset` at the same instant.
  */
def dLatch(in: Port, clk: Port, clear: Port = High, preset: Port = High): Spec[(Port, Port)] = newSpec {
  val (clkM, clkS) = nonOverlapClock(clk)
  dLatchWithPhases(in, clkM, clkS, clear, preset)
}

/** Same as [[dLatch]], but takes the two phase signals from a shared `nonOverlapClock` instead of building its own. Use
  * this when many flip-flops share one clock (e.g. [[ringCounter]]) so there is only one phase generator instead of one
  * per bit.
  */
private[component] def dLatchWithPhases(
    in: Port,
    clkM: Port,
    clkS: Port,
    clear: Port = High,
    preset: Port = High
): Spec[(Port, Port)] =
  newSpec {
    val (qm, _) = latchClocked(in, not(in), clkM, clear, preset)
    latchClocked(qm, not(qm), clkS, clear, preset)
  }

/** A JK flip-flop, for counters (page 99).
  *
  * Built from an edge-triggered [[dLatch]] driven by the JK next-state equation `next = (j & ~q) | (~k & q)`:
  * `j=1, k=0` sets, `j=0, k=1` resets, `j=k=1` toggles, `j=k=0` holds. Because the D flip-flop captures atomically on
  * the rising edge, `j`/`k` may change freely while the clock is Low (subject to the usual setup time).
  *
  * Same timing contract as [[dLatch]]: `j`/`k` stable around the rising clock edge; keep the clock slow enough for all
  * signals to settle between edges; do not toggle `clear` and `preset` at the same instant.
  */
def jkFlipFlop(j: Port, k: Port, clk: Port, clear: Port, preset: Port = High): Spec[(Port, Port)] = newSpec {
  val qAux, nqAux = newPort()
  val (q, nq) = dLatch(or(and(j, nqAux), and(not(k), qAux)), clk, clear, preset)
  q ~> qAux
  nq ~> nqAux
  (q, nq)
}

/** A controlled buffer register (page 107). */
def register(xs: Bus, load: Port, clk: Port, clear: Port = High): Spec[Bus] = newSpec {
  val notLoad = not(load)
  xs.map { x =>
    val aux = newPort()
    val (q, _) = dLatch(or(and(aux, notLoad), and(x, load)), clk, clear = clear)
    q ~> aux
    q
  }
}

/** A negative edge-triggered `n`-bit controlled counter (page 113).
  */
def counter(n: Int, count: Port, clk: Port, clear: Port): Spec[Bus] = newSpec {
  (1 to n).scanLeft(clk) { case (prev, _) => jkFlipFlop(count, count, not(prev), clear)._1 }.tail.toVector
}

/** A negative edge-triggered presettable counter (page 119).
  */
def presettableCounter(ps: Bus, load: Port, clk: Port, clear: Port): Spec[Bus] = newSpec {
  ps.scanLeft(clk) { case (prev, p) =>
    jkFlipFlop(High, High, not(prev), clear = and(clear, nand(load, not(p))), preset = or(not(clear), nand(load, p)))._1
  }.tail
}

/** A negative edge-triggered `n`-bit ring counter (page 116).
  *
  * `clear` presets stage 0 and clears the others, so the ring starts at 100…0.
  */
def ringCounter(n: Int, clk: Port, clear: Port): Spec[Bus] = newSpec {
  val clkBar = not(clk)
  val (clkM, clkS) = nonOverlapClock(clkBar)
  val ds = Vector.fill(n)(newPort())
  val qs = Vector.tabulate(n) { i =>
    val (q, _) =
      if (i == 0) dLatchWithPhases(ds(i), clkM, clkS, preset = clear)
      else dLatchWithPhases(ds(i), clkM, clkS, clear = clear)
    q
  }
  qs.last ~> ds(0)
  for (i <- 1 until n) qs(i - 1) ~> ds(i)
  qs
}

/** A static RAM with 2 ^ `addr.length` words of `ins.length` bits. The RAM is controlled and buffered, allowing read
  * and write activation.
  *
  * Contract: the address and the mode must be stable while `we` is High. Each cell is gated by `and(we, sel)`, and the
  * decoder behind `sel` glitches when the address changes; letting a cell's gate fall shortly after a data change can
  * leave the cell oscillating instead of settling, which hangs the simulator rather than corrupting a word. (Changing
  * the address while write is enabled is a violation for a real SRAM too.)
  */
def ram(ins: Bus, addr: Bus, we: Port, ce: Port): Spec[Bus] = newSpec {
  val select = decoder(addr, High)
  val outs = ins.map { in =>
    select.map { sel => and(sel, latchClocked(in, not(in), and(we, sel))._1) }.reduce(or)
  }
  buffered(ce)(outs)
}

/** A ROM with `data.length` words of `data(0).length` bits. The address bus size should be compatible with the number
  * of words in `data`.
  */
def rom(data: Seq[Seq[Boolean]], addr: Bus): Spec[Bus] = newSpec {
  assert(data.length == 1 << addr.length, "Data and address bus sizes do not match")

  val select = decoder(addr, High)
  data.transpose.map { col =>
    select.zip(col).map { (sel, v) => and(sel, if (v) High else Low) }.reduce(or)
  }.toVector
}

/** A sum-of-products term for [[pla]]: a pattern over the inputs, plus the outputs it drives.
  *
  * `pattern(i)` is the literal for input `i`: `Some(true)` requires the input high, `Some(false)` requires it low,
  * `None` is don't-care (the input is left out of the product). `outputs` holds the indices of the outputs this term
  * drives.
  */
case class Term(pattern: Vector[Option[Boolean]], outputs: Set[Int])

/** Sum-of-products PLA: the shape of the 6502's decode ROM.
  *
  * Each term is built as an AND over its literals (don't-cares are left out of the product); each output is an OR over
  * the terms that drive it. The input inverters are built once and shared by all terms; an output with no terms is tied
  * low.
  *
  * The term table is validated: every pattern must be as wide as `inputs`, and every driven output must lie in
  * `[0, outputs)`.
  */
def pla(inputs: Bus, terms: Seq[Term], outputs: Int): Spec[Bus] = newSpec {
  assert(outputs >= 0, s"PLA output count must be non-negative, got $outputs")
  terms.foreach { t =>
    assert(
      t.pattern.length == inputs.length,
      s"Term pattern width ${t.pattern.length} does not match input width ${inputs.length}"
    )
    t.outputs.foreach { o =>
      assert(o >= 0 && o < outputs, s"Term drives output $o, outside [0, $outputs)")
    }
  }

  val notInputs = inputs.map(not)
  val termPorts = terms.map { t =>
    val literals = t.pattern.zipWithIndex.collect {
      case (Some(true), i) => inputs(i)
      case (Some(false), i) => notInputs(i)
    }
    if (literals.isEmpty) High else literals.reduce(and)
  }
  (0 until outputs).map { o =>
    val drivers = termPorts.zip(terms).collect { case (p, t) if t.outputs.contains(o) => p }
    if (drivers.isEmpty) Low else drivers.reduce(or)
  }.toVector
}

/** A ROM like [[rom]], but smaller: constant-zero bits are skipped instead of built (no `and(sel, Low)` gates and
  * smaller OR trees), and sparse contents are emitted as a [[pla]] instead. Like [[rom]], the address decoder is built
  * once and shared across all output bits.
  *
  * Which structure is smaller is decided by a 2-input-gate heuristic on the number of 1-bits: the folded decoder spends
  * about `2^(n+1)` ANDs on the address decoder, while a PLA spends about `n-1` ANDs per 1-bit minterm; the output OR
  * trees cost the same either way, so the 1-bit count alone picks the winner.
  *
  * Same contract as [[rom]]: `data.length` must be `2^addr.length`, and all words must have the same width. [[rom]]
  * itself is left untouched for SAP-1.
  */
def romFolded(data: Seq[Seq[Boolean]], addr: Bus): Spec[Bus] = newSpec {
  assert(data.length == 1 << addr.length, "Data and address bus sizes do not match")
  val width = data.head.length
  assert(data.forall(_.length == width), "All data words must have the same width")

  val totalOnes = data.foldLeft(0)(_ + _.count(identity))
  val decoderAnds = (data.length << 1) - 2
  val mintermAnds = totalOnes * (addr.length - 1).max(0)
  if (mintermAnds <= decoderAnds) {
    // Sparse contents: one minterm per 1-bit, as a PLA.
    val terms = data.zipWithIndex.flatMap { (word, w) =>
      val pattern = Vector.tabulate(addr.length)(i => Some(((w >> i) & 1) == 1))
      word.zipWithIndex.collect { case (true, b) => Term(pattern, Set(b)) }
    }
    pla(addr, terms, width)
  } else {
    // Dense contents: one shared decoder, constant-zero bits skipped.
    val select = decoder(addr, High)
    data.transpose.map { col =>
      val ones = select.zip(col).collect { case (sel, true) => sel }
      if (ones.isEmpty) Low else ones.reduce(or)
    }.toVector
  }
}
