package component.sap1

import component.BuilderAPI._
import component._
import core._

import ControlBus._
import ControlBus.Bit._

case class Input(prog: Port, write: Port, addr: Bus, data: Bus)

case class ControlBus(con: Bus) {
  def apply(b: ControlBus.Bit) = con(b.ordinal)
}

object ControlBus {
  enum Bit { case Lo, Lb, Eu, Su, Ea, La, Ei, Li, Ce, Lm, Ep, Cp }

  def fromBits(bits: Bit*): Vector[Boolean] =
    bits.foldLeft(Vector.fill(12)(false)) { (vec, b) => vec.updated(b.ordinal, true) }
}

def sap1(clkSig: Port, clr: Port, ramIn: Input): Spec[(Port, Bus)] = newSpec {
  val bus = newBus(8)

  val instr = newBus(4)
  val hlt = multi(and)(instr: _*)
  val clk = clock(clkSig, clr, hlt)
  val con = sequencer(instr, clk, clr)

  instrRegister(bus, con(Li), clk, clr, con(Ei)) ~> instr

  progCounter(bus, con(Cp), not(clk), clr, con(Ep))

  val mOut = inputAndMar(bus, con(Lm), ramIn, clk)
  ram(bus, mOut, con(Ce), ramIn)

  val aOut = accumulator(bus, clk, con(La), con(Ea))
  val bOut = register(bus, con(Lb), clk)
  alu(bus, aOut, bOut, con(Su), con(Eu))

  val oOut = register(bus, con(Lo), clk)
  (hlt, oOut)
}

def clock(clkSig: Port, clr: Port, hlt: Port): Spec[Port] = newSpec {
  jkFlipFlop(not(hlt), not(hlt), clkSig, clr)._1
}

def sequencer(instr: Bus, clk: Port, clr: Port): Spec[ControlBus] = newSpec {
  val Vector(t1, t2, t3, t4, t5, t6) = ringCounter(6, clk, clr)
  val Vector(lda, add, sub, _, _, _, _, _, _, _, _, _, _, _, out, _) = decoder(instr, High)

  ControlBus(
    Vector(
      multi(or)(and(t4, out)),
      multi(or)(and(t5, add), and(t5, sub)),
      multi(or)(and(t6, add), and(t6, sub)),
      multi(or)(and(t6, sub)),
      multi(or)(and(t4, out)),
      multi(or)(and(t5, lda), and(t6, add), and(t6, sub)),
      multi(or)(and(t4, lda), and(t4, add), and(t4, sub)),
      multi(or)(t3),
      multi(or)(t3, and(t5, lda), and(t5, add), and(t5, sub)),
      multi(or)(t1, and(t4, lda), and(t4, add), and(t4, sub)),
      multi(or)(t1),
      multi(or)(t2)
    )
  )
}

def microprogSequencer(instr: Bus, clk: Port, clr: Port): Spec[ControlBus] = newSpec {
  val t = ringCounter(6, clk, clr)
  val addr1 = addrRom(instr)
  val addr2 = presettableCounter(addr1, t(2), clk, and(clr, not(posEdge(t(0)))))
  ControlBus(controlRom(addr2))
}

def instrRegister(bus: Bus, load: Port, clk: Port, clr: Port, enable: Port): Spec[Bus] = newSpec {
  val (bus0, bus1) = bus.splitAt(4)
  buffered(enable)(register(bus1, load, clk)) ~> bus1
  register(bus0, load, clk, clr)
}

def progCounter(bus: Bus, count: Port, clk: Port, clr: Port, enable: Port): Spec[Unit] = newSpec {
  buffered(enable)(counter(4, count, clk, clr)) ~> bus.drop(4)
}

def inputAndMar(bus: Bus, load: Port, ramIn: Input, clk: Port): Spec[Bus] = newSpec {
  val mOut = register(bus.drop(4), load, clk)
  mOut.zip(ramIn.addr).map { case (out, a) => mux(Vector(out, a), Vector(ramIn.prog)) }
}

def ram(bus: Bus, addr: Bus, ce: Port, ramIn: Input): Spec[Unit] = newSpec {
  component.ram(ramIn.data, addr, ramIn.write, and(ce, not(ramIn.prog))) ~> bus
}

def accumulator(bus: Bus, clk: Port, load: Port, enable: Port): Spec[Bus] = newSpec {
  val outs = register(bus, load, clk)
  buffered(enable)(outs) ~> bus
  outs
}

def alu(bus: Bus, ins1: Bus, ins2: Bus, sub: Port, enable: Port): Spec[Unit] = newSpec {
  buffered(enable)(addSub(ins1, ins2, sub)) ~> bus
}

def addrRom(addr: Bus): Spec[Bus] = newSpec {
  def toBin(x: Int, n: Int): Seq[Boolean] = (0 until n).map(i => (x & (1 << i)) != 0)

  // format: off
  val data = (0 to 15).map {
    case 0 => toBin(3, 4)     // LDA
    case 1 => toBin(6, 4)     // ADD
    case 2 => toBin(9, 4)     // SUB
    case 14 => toBin(12, 4)   // OUT
    case _ => toBin(0, 4)     // Not used
  }
  // format: on
  rom(data, addr)
}

def controlRom(addr: Bus): Spec[Bus] = newSpec {
  // format: off
  val data = Vector(
    ControlBus.fromBits(Ep, Lm),      // t1
    ControlBus.fromBits(Cp),          // t2
    ControlBus.fromBits(Ce, Li),      // t3
    ControlBus.fromBits(Lm, Ei),      // t4 LDA
    ControlBus.fromBits(Ce, La),      // t5 LDA
    ControlBus.fromBits(),            // t6 LDA
    ControlBus.fromBits(Lm, Ei),      // t4 ADD
    ControlBus.fromBits(Ce, Lb),      // t5 ADD
    ControlBus.fromBits(La, Eu),      // t6 ADD
    ControlBus.fromBits(Lm, Ei),      // t4 SUB
    ControlBus.fromBits(Ce, Lb),      // t5 SUB
    ControlBus.fromBits(La, Su, Eu),  // t6 SUB
    ControlBus.fromBits(Ea, Lo),      // t4 OUT
    ControlBus.fromBits(),            // t5 OUT
    ControlBus.fromBits(),            // t6 OUT
    ControlBus.fromBits()             // Not used
  )
  // format: on
  rom(data, addr)
}
