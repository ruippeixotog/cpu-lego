package component

import component.BuilderAPI.*
import core.*

/** Textbook active-low SR latch: two NAND gates wired in a loop (page 92).
  *
  * This is the raw memory cell everything else is built from. It is not safe to use directly: driving both
  * inputs Low is forbidden (both outputs go High, and releasing them together can leave the latch oscillating
  * instead of settling). Its inputs must also change one at a time.
  *
  * Academic building block — use [[latchClocked]] (which adds clock gating and override priority) or [[dLatch]]
  * instead.
  */
def nandLatch(set: Port, reset: Port): Spec[(Port, Port)] = newSpec {
  val aux1, aux2 = newPort()
  val q = nand(set, aux1)
  val nq = nand(reset, aux2)
  q ~> aux2
  nq ~> aux1
  (q, nq)
}

/** Textbook active-high SR latch: two NOR gates wired in a loop (the NORs are themselves built from NANDs).
  *
  * Same warning as [[nandLatch]]: driving both inputs High is forbidden. Academic building block — used here
  * only as the Yosys `$_SR_PP_` cell mapping. Prefer [[latchClocked]] or [[dLatch]].
  */
def norLatch(set: Port, reset: Port): Spec[(Port, Port)] = newSpec {
  val aux1, aux2 = newPort()
  val q = nor(reset, aux1)
  val nq = nor(set, aux2)
  q ~> aux2
  nq ~> aux1
  (q, nq)
}

/** Recommended low-level storage primitive: a clocked SR latch with asynchronous overrides (pages 94, 97).
  *
  * While `clk` is High the latch follows `set`/`reset`; while Low it holds its value. The active-low `clear`
  * and `preset` override everything at any time: `clear` forces the output Low, `preset` forces it High
  * (`clear` wins if both are pressed). A priority circuit in front of the raw [[nandLatch]] makes sure it never
  * sees the forbidden "both Low" combination, no matter what the overrides do.
  *
  * Contract: do not drive `set` and `reset` High together while `clk` is High; do not drive `clear` and
  * `preset` Low together; change inputs one at a time.
  */
def latchClocked(set: Port, reset: Port, clk: Port, clear: Port = High, preset: Port = High): Spec[(Port, Port)] =
  newSpec {
    nandLatch(
      or(not(clear), and(preset, nand(set, clk))),
      or(not(preset), and(clear, nand(reset, clk)))
    )
  }

/** Splits the clock into two phase signals for master-slave flip-flops.
  *
  * Produces `(clkM, clkS)`: `clkM` is High while `clk` is Low (the master latch may read its input), `clkS` is
  * High while `clk` is High (the slave latch may read the master). Each phase turns on a little later than the
  * other turns off, so the master is fully closed before the slave opens, and vice versa. The two latches are
  * therefore never open at the same time — without this, input data could shoot straight through both latches
  * within a single clock phase and the flip-flop would stop behaving as edge-triggered.
  *
  * The "one closes before the other opens" ordering holds for any positive gate/wire delays: the turn-on path
  * passes through strictly more gates than the turn-off path, so it is strictly slower.
  */
def nonOverlapClock(clk: Port): Spec[(Port, Port)] = newSpec {
  val clkM = and(not(clk), not(not(not(clk))))
  val clkS = and(clk, not(not(clk)))
  (clkM, clkS)
}

/** Recommended flip-flop: captures `in` on the rising edge of `clk` (page 96).
  *
  * Two [[latchClocked]]s in series (master and slave), driven by the two phases of [[nonOverlapClock]]. The
  * master reads `in` while the clock is Low; on the rising edge the master closes and the slave opens, copying
  * the master to the output. Because the phases never overlap, the output only changes on the rising edge.
  *
  * The active-low `clear`/`preset` override at any time (`clear` wins if both are pressed); they reach both
  * latches, so no stale value survives in the master.
  *
  * Timing contract: keep `in` stable around the rising clock edge; keep the clock slow enough for all signals
  * to settle between edges; do not toggle `clear` and `preset` at the same instant.
  */
def dLatch(in: Port, clk: Port, clear: Port = High, preset: Port = High): Spec[(Port, Port)] = newSpec {
  val (clkM, clkS) = nonOverlapClock(clk)
  dLatchWithPhases(in, clkM, clkS, clear, preset)
}

/** Same as [[dLatch]], but takes the two phase signals from a shared [[nonOverlapClock]] instead of building its
  * own. Use this when many flip-flops share one clock (e.g. [[ringCounter]]) so there is only one phase
  * generator instead of one per bit.
  */
def dLatchWithPhases(in: Port, clkM: Port, clkS: Port, clear: Port = High, preset: Port = High): Spec[(Port, Port)] =
  newSpec {
    val (qm, _) = latchClocked(in, not(in), clkM, clear, preset)
    latchClocked(qm, not(qm), clkS, clear, preset)
  }

/** Recommended JK flip-flop, for counters (page 99).
  *
  * Master-slave structure like [[dLatch]]: the master reads `j`/`k` (combined with the slave's feedback) while
  * the clock is Low, the slave copies the master while the clock is High. `j=1, k=0` sets, `j=0, k=1` resets,
  * `j=k=1` toggles. The feedback comes from the slave, which only changes while the master is closed, so the
  * output cannot race around within one clock phase.
  *
  * Same timing contract as [[dLatch]]: `j`/`k` stable around the rising edge, slow enough clock, and do not
  * toggle `clear`/`preset` at the same instant.
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
