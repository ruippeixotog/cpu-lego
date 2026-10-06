package component

import component.BuilderAPI.*
import core.*

/** A synchronous clock domain: one shared two-phase clock generator for all of the kit's storage elements.
  *
  * `clkM`/`clkS` are the two phases of a single [[nonOverlapClock]] built from `clk`, exposed for the kit (`reg`,
  * [[syncCounter]], [[shiftReg]]) so that every flip-flop in the domain shares one generator instead of each bit
  * building its own (about 10 NANDs saved per bit).
  *
  * `resetSync` is the domain's asynchronous reset, active High. Asserting `resetN` (Low) clears the two synchroniser
  * flip-flops immediately — assertion is asynchronous. On release, the de-asserted level takes two rising clock edges
  * to propagate, so every flip-flop in the domain sees the release on the same clock edge. Wire it to [[reg]]'s
  * `syncReset`.
  */
case class ClockDomain(clk: Port, clkM: Port, clkS: Port, resetSync: Port)

/** Builds a [[ClockDomain]] from `clk`: one shared [[nonOverlapClock]] plus a two-flip-flop reset synchroniser on the
  * active-low `resetN` pin (`resetN = High`, the default, disables the reset).
  *
  * Use the asynchronous reset for power-on reset only. Everything else — including resets raised while the machine is
  * running — must use the synchronous `syncReset` of [[reg]]: releasing an asynchronous reset near a clock edge can
  * otherwise leave different flip-flops resetting on different edges.
  */
def clockDomain(clk: Port, resetN: Port = High): Spec[ClockDomain] = newSpec {
  val (clkM, clkS) = nonOverlapClock(clk)
  val (s1, _) = dLatchWithPhases(High, clkM, clkS, clear = resetN)
  val (s2, _) = dLatchWithPhases(s1, clkM, clkS, clear = resetN)
  ClockDomain(clk, clkM, clkS, not(s2))
}

/** A synchronous register: captures `d` on the rising edge of the domain clock.
  *
  * The enable is a load multiplexer (`q' = en ? d : q`) sampled by the flip-flop — it never gates the clock. The
  * synchronous reset wins over the enable (`q' = syncReset ? resetBit : (en ? d : q)`), with each bit resetting to the
  * corresponding bit of `resetValue`. For the asynchronous (power-on) reset, pass the domain's `resetSync` as
  * `syncReset`.
  *
  * Constant enables and resets (`High`/`Low`) are folded away instead of built as gates.
  *
  * Timing contract: `d`, `en` and `syncReset` must be stable around the rising clock edge.
  */
def reg(d: Bus, en: Port = High, syncReset: Port = Low, resetValue: Long = 0)(using
    dom: ClockDomain
): Spec[Bus] = newSpec {
  val resetBits = const(d.length, resetValue)
  // Shared by all bits; constant selects are folded away rather than built as gates.
  val enN = if (en eq High) None else Some(not(en))
  val rstN = if (syncReset eq Low) None else Some(not(syncReset))

  d.zip(resetBits)
    .map { case (di, resetBit) =>
      val qAux = newPort()
      val loaded = enN match {
        case None => di
        case Some(enN0) => or(and(en, di), and(enN0, qAux))
      }
      val next = rstN match {
        case None => loaded
        case Some(rstN0) =>
          if (resetBit eq High) or(syncReset, loaded)
          else and(rstN0, loaded)
      }
      val (q, _) = dLatchWithPhases(next, dom.clkM, dom.clkS)
      q ~> qAux
      q
    }
    .toVector
}

/** A synchronous binary counter: every bit is clocked by the domain clock — no ripple.
  *
  * With `en` High, each rising edge advances the count by one (wrapping around); with `en` Low the count holds. A High
  * `load` synchronously loads `loadValue` on the rising edge, winning over both counting and `en`.
  *
  * The counter powers up unset like any flip-flop: drive `load` (wired to the domain's `resetSync` for power-on reset)
  * to reach a known state.
  *
  * Timing contract: `en`, `load` and the domain clock follow the [[reg]] contract.
  */
def syncCounter(width: Int, en: Port = High, load: Port = Low, loadValue: Long = 0)(using
    dom: ClockDomain
): Spec[Bus] = newSpec {
  assert(width >= 0, s"syncCounter width must be non-negative, got $width")
  val qs = Vector.fill(width)(newPort())
  val loadBits = const(width, loadValue)
  val loadN = if (load eq Low) None else Some(not(load))

  // carry(i) is the AND of qs(0) .. qs(i-1); bit i toggles when carry(i) is High.
  val carries = qs.scanLeft(High: Port) { (c, q) => if (c eq High) q else and(c, q) }
  val next = qs.zip(loadBits).zipWithIndex.map { case ((q, loadBit), i) =>
    val toggled = if (carries(i) eq High) not(q) else xor(q, carries(i))
    loadN match {
      case None => toggled
      case Some(loadN0) =>
        if (loadBit eq High) or(load, toggled)
        else and(loadN0, toggled)
    }
  }
  // `load` wins over `en`: the register captures whenever either is High.
  val capture =
    if ((en eq High) || (load eq High)) High
    else if (load eq Low) en
    else if (en eq Low) load
    else or(en, load)
  val out = reg(next, capture)
  out ~> qs
  out
}

/** A synchronous shift register.
  *
  * On each rising clock edge with `en` High, every bit takes its neighbour's value: toward the MSB when `dir` is Low
  * (`in` enters at bit 0), toward the LSB when `dir` is High (`in` enters at the top bit). With `en` Low the contents
  * hold.
  *
  * Like any flip-flop it powers up unset: shift in `width` known bits (or hold `en` Low and accept the unknown initial
  * contents) to reach a known state.
  *
  * Timing contract: `in`, `en`, `dir` and the domain clock follow the [[reg]] contract.
  */
def shiftReg(width: Int, in: Port, en: Port = High, dir: Port = Low)(using dom: ClockDomain): Spec[Bus] =
  newSpec {
    assert(width >= 0, s"shiftReg width must be non-negative, got $width")
    val qs = Vector.fill(width)(newPort())
    val dirN = if ((dir eq Low) || (dir eq High)) None else Some(not(dir))

    val next = qs.zipWithIndex.map { case (q, i) =>
      val up = if (i == 0) in else qs(i - 1) // dir Low: shift toward the MSB
      val down = if (i == width - 1) in else qs(i + 1) // dir High: shift toward the LSB
      dirN match {
        case None =>
          if (dir eq Low) up else down
        case Some(dirN0) => or(and(dirN0, up), and(dir, down))
      }
    }
    val out = reg(next, en)
    out ~> qs
    out
  }
