package component

import component.BuilderAPI.*
import core.*

/** Non-intrinsic version of `flipflop`. A race condition occurs when both `set` and `reset` are Low (page 92).
  */
def nandLatch(set: Port, reset: Port): Spec[(Port, Port)] = newSpec {
  val aux1, aux2 = newPort()
  val q = nand(reset, aux1)
  val nq = nand(set, aux2)
  q ~> aux2
  nq ~> aux1
  (q, nq)
}

/** A flip-flop with positive (level) clocking as well as asynchronous, active low `clear` and `preset` signals (pages
  * 94 and 97).
  */
def latchClocked(set: Port, reset: Port, clk: Port, clear: Port = High, preset: Port = High): Spec[(Port, Port)] =
  newSpec {
    flipflop(
      and(clear, nand(reset, clk)),
      and(preset, nand(set, clk))
    )
  }

/** A positive edge triggered D flip-flop with asynchronous signals (pages 96).
  */
def dLatch(in: Port, clk: Port, clear: Port = High, preset: Port = High): Spec[(Port, Port)] = newSpec {
  latchClocked(in, not(in), posEdge(clk), clear, preset)
}

/** A positive edge triggered JK flip-flop with asynchronous `clear` (page 99).
  */
def jkFlipFlop(j: Port, k: Port, clk: Port, clear: Port, preset: Port = High): Spec[(Port, Port)] = newSpec {
  val aux1, aux2 = newPort()
  val (q, nq) = latchClocked(and(aux1, j), and(aux2, k), posEdge(clk), clear = clear, preset = preset)
  nq ~> aux1
  q ~> aux2
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
  val aux1, aux2 = newPort()
  val first = jkFlipFlop(aux1, aux2, not(clk), clear).swap
  val outs = (2 to n).scanLeft(first) { case ((q, nq), _) => jkFlipFlop(q, nq, not(clk), clear) }
  outs.last._1 ~> aux2
  outs.last._2 ~> aux1
  outs.map(_._1).toVector
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
