package component

import component.BuilderAPI.*
import core.*

/** An active-low SR latch built from two cross-coupled NAND gates (page 92).
  *
  * `set` and `reset` are active-low: holding `set` Low drives `q` High, holding `reset` Low drives `q` Low.
  * Holding both High retains the previous value. Driving both Low is the forbidden state (both outputs go High)
  * and must be avoided; the latch is only guaranteed to settle correctly when its inputs change one at a time
  * (fundamental mode).
  */
def nandLatch(set: Port, reset: Port): Spec[(Port, Port)] = newSpec {
  val aux1, aux2 = newPort()
  val q = nand(set, aux1)
  val nq = nand(reset, aux2)
  q ~> aux2
  nq ~> aux1
  (q, nq)
}

/** An active-high SR latch built from two cross-coupled NOR gates (which are themselves built from NAND gates).
  *
  * `set` and `reset` are active-high: holding `set` High drives `q` High, holding `reset` High drives `q` Low.
  * Holding both Low retains the previous value. Driving both High is the forbidden state (both outputs go Low)
  * and must be avoided; the latch is only guaranteed to settle correctly when its inputs change one at a time
  * (fundamental mode).
  */
def norLatch(set: Port, reset: Port): Spec[(Port, Port)] = newSpec {
  val aux1, aux2 = newPort()
  val q = nor(reset, aux1)
  val nq = nor(set, aux2)
  q ~> aux2
  nq ~> aux1
  (q, nq)
}

/** A flip-flop with positive (level) clocking as well as asynchronous, active low `clear` and `preset` signals (pages
  * 94 and 97).
  *
  * Built from a [[nandLatch]] whose active-low pins are driven by the clocked data path. The `or(not(clear), ...)`
  * terms give the asynchronous signals priority over the data path: without them, asserting `clear` (or `preset`)
  * while the data path is active would drive both latch pins Low, hitting the forbidden state.
  *
  * The `or` structure keeps async transitions monotonic in the common case: `and(preset, nand(set, clk))` does not
  * depend on `clear`, so when only `clear` changes, only the `not(clear)` term moves. If `set`/`clk` change at the
  * same instant as `clear`, a glitch is possible — async inputs must respect recovery/removal times.
  *
  * Contract (same as the physical 7475): do not drive `set` and `reset` High together while `clk` is High, and do
  * not drive `clear` and `preset` Low together. Inputs must change one at a time (fundamental mode) for the
  * underlying NAND latch to settle deterministically.
  */
def latchClocked(set: Port, reset: Port, clk: Port, clear: Port = High, preset: Port = High): Spec[(Port, Port)] =
  newSpec {
    nandLatch(
      or(not(clear), and(preset, nand(set, clk))),
      or(not(preset), and(clear, nand(reset, clk)))
    )
  }

/** Non-overlapping two-phase clock generator for master-slave flip-flops.
  *
  * Produces `(clkM, clkS)` where `clkM` is High while `clk` is Low (master phase) and `clkS` is High while `clk`
  * is High (slave phase). Each phase's rising edge is delayed by two gate delays via `and(x, not(not(x)))`, while
  * its falling edge follows the input directly. On a rising `clk`, the master phase falls promptly (master
  * closes) and the slave phase rises only after the delay (slave opens); on a falling `clk`, the reverse holds.
  *
  * Design intent: the closing latch shuts before the other opens (break-before-make), so the two latches of a
  * master-slave pair are not transparent simultaneously under the nominal delay model (positive gate delays,
  * the delayed path strictly slower than the direct path). This has been analyzed for the nominal topology but
  * is not formally proven against process variation or the simulator's exact event scheduling; it is an
  * experimental structure. If the non-overlap fails, a combinational loop can form through the pair.
  */
def nonOverlapClock(clk: Port): Spec[(Port, Port)] = newSpec {
  val clkM = and(not(clk), not(not(not(clk))))
  val clkS = and(clk, not(not(clk)))
  (clkM, clkS)
}

/** A positive edge triggered D flip-flop with asynchronous signals (pages 96).
  *
  * Master-slave structure built from two [[latchClocked]]s driven by [[nonOverlapClock]] phases: the master is
  * transparent while `clk` is Low and captures `in`; the slave is transparent while `clk` is High and captures
  * the master, so `q` only changes on the rising edge. The non-overlapping phases are intended to ensure the
  * master closes before the slave opens (and vice versa), avoiding the transparency overlap that would allow
  * race-around.
  *
  * Complement skew: `in`/`not(in)` (and `qm`/`not(qm)`) arrive at different times, transiently exposing the
  * forbidden latch input combination. This is contained because the skew occurs while the downstream latch is
  * opaque (the master skews while the slave is closed, and vice versa), so the transient does not propagate —
  * provided the upstream latch has settled before the downstream opens, i.e. inputs respect setup/hold around
  * the closing edge. If setup/hold is violated, the latch can enter metastability like any bistable; this
  * design does not eliminate metastability, it provides the standard synchronous timing contract.
  *
  * The asynchronous `clear`/`preset` go to both latches: otherwise the master would retain stale state and corrupt
  * the slave when the asynchronous signal is released.
  *
  * Timing contract: `in` must be stable for setup/hold around the rising edge of `clk`; `clk` high/low phases
  * must exceed the latch propagation delay plus the phase-generator dead time; async `clear`/`preset` require
  * recovery/removal around the clock edge. No unconditional guarantee is made — see [[nonOverlapClock]] for
  * the experimental status of the phase generator.
  */
def dLatch(in: Port, clk: Port, clear: Port = High, preset: Port = High): Spec[(Port, Port)] = newSpec {
  val (clkM, clkS) = nonOverlapClock(clk)
  dLatchWithPhases(in, clkM, clkS, clear, preset)
}

/** A D flip-flop taking pre-generated non-overlapping phases (for sharing one generator across many bits). */
def dLatchWithPhases(in: Port, clkM: Port, clkS: Port, clear: Port = High, preset: Port = High): Spec[(Port, Port)] =
  newSpec {
    val (qm, _) = latchClocked(in, not(in), clkM, clear, preset)
    latchClocked(qm, not(qm), clkS, clear, preset)
  }

/** A positive edge triggered JK flip-flop with asynchronous `clear` (page 99).
  *
  * Master-slave structure driven by [[nonOverlapClock]] phases: the master captures `j`/`k` (gated by the slave's
  * feedback) while `clk` is Low; the slave captures the master while `clk` is High. The feedback comes from the
  * slave, which only changes while the master is opaque (under the non-overlap intent), avoiding race-around.
  * The master's inputs `j & ~q` and `k & q` cannot both be High when the slave outputs are settled complementary;
  * transient skew is contained by the opaque phase as in [[dLatch]], subject to the same setup/hold contract.
  *
  * The asynchronous `clear`/`preset` go to both latches: otherwise the master would retain stale state and corrupt
  * the slave when the asynchronous signal is released.
  *
  * Timing contract: as for [[dLatch]] — `j`/`k` need setup/hold around the rising edge, phases must exceed
  * latch delay plus dead time, async signals need recovery/removal. No unconditional metastability guarantee.
  */
def jkFlipFlop(j: Port, k: Port, clk: Port, clear: Port, preset: Port = High): Spec[(Port, Port)] = newSpec {
  val (clkM, clkS) = nonOverlapClock(clk)
  val qm = newPort()
  val (q, nq) = latchClocked(qm, not(qm), clkS, clear, preset)
  val (qmOut, _) = latchClocked(and(j, nq), and(k, q), clkM, clear, preset)
  qmOut ~> qm
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
  */
def ringCounter(n: Int, clk: Port, clear: Port): Spec[Bus] = newSpec {
  // D-flip-flop based ring counter (shift register with feedback).
  // On active-low clear: stage 0 is preset to 1, others cleared to 0,
  // giving the initial 100...0 pattern. On each clock, the 1 shifts right.
  // Uses D flip-flops instead of JK to simplify the feedback path.
  // Shares a single non-overlapping clock generator across all stages to
  // reduce gate count and simulator events.
  val clkBar = not(clk)
  val (clkM, clkS) = nonOverlapClock(clkBar)
  // Create the D input ports first, then wire them in a ring
  val ds = Vector.fill(n)(newPort())
  val qs = Vector.tabulate(n) { i =>
    val (q, _) =
      if (i == 0) dLatchWithPhases(ds(i), clkM, clkS, clear = High, preset = clear)
      else dLatchWithPhases(ds(i), clkM, clkS, clear = clear)
    q
  }
  // Wire: D(0) = Q(n-1), D(i) = Q(i-1)
  qs.last ~> ds(0)
  for (i <- 1 until n) qs(i - 1) ~> ds(i)
  qs
}

/** A static RAM with 2 ^ `addr.length` words of `ins.length` bits. The RAM is controlled and buffered, allowing read
  * and write activation.
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
