package component.sap1

import component.BuilderAPI._
import component._
import core._

case class Input(prog: Port, write: Port, addr: Bus, data: Bus, clr: Port, step: Port, auto: Port)

case class ControlBus(con: Bus, clk: Port, clr: Port) {
  val Vector(cp, ep, lm, ce, li, ei, la, ea, su, eu, lb, lo) = con
}

def sap1(sapIn: Input): Spec[Bus] = newSpec {
  val bus = newBus(8)

  val instr = newBus(4)
  val ctrl = controller(sapIn, instr)
  import ctrl._

  instrRegister(bus, li, clk, clr, ei) ~> instr

  progCounter(bus, cp, not(clk), not(clr), ep)

  val mOut = register(bus.take(4), lm, clk)
  ram(bus, mOut, ce, sapIn)

  val aOut = accumulator(bus, clk, la, ea)
  val bOut = register(bus, lb, clk)
  alu(bus, aOut, bOut, su, eu)

  register(bus, lo, clk)
}

def controller(sapIn: Input, instr: Bus): Spec[ControlBus] = newSpec {
  val nhlt = not(multi(and)(instr: _*))

  val (clr, _) = flipflop(sapIn.clr, not(sapIn.clr))
  val (step, _) = flipflop(sapIn.step, not(sapIn.step))
  val (manual, auto) = flipflop(not(sapIn.auto), sapIn.auto)
  val clk = nand(
    nand(manual, and(nhlt, step)),
    nand(auto, jkFlipFlop(nhlt, nhlt, clock(10000), clr)._1)
  )

  def toBin(x: Int, n: Int): Seq[Boolean] = (0 until n).map(i => (x & (1 << i)) != 0)

  val addrRomData = (0 to 15).map {
    case 0 => toBin(3, 4) // LDA
    case 1 => toBin(6, 4) // ADD
    case 2 => toBin(9, 4) // SUB
    case 14 => toBin(12, 4) // OUT
    case _ => toBin(0, 4) // Not used
  }

  val controlRomData = Vector(
    toBin(0x5e3, 12), // t1
    toBin(0xbe3, 12), // t2
    toBin(0x263, 12), // t3
    toBin(0x1a3, 12), // t4 LDA
    toBin(0x2c3, 12), // t5 LDA
    toBin(0x3e3, 12), // t6 LDA
    toBin(0x1a3, 12), // t4 ADD
    toBin(0x2e1, 12), // t5 ADD
    toBin(0x3c7, 12), // t6 ADD
    toBin(0x1a3, 12), // t4 SUB
    toBin(0x2e1, 12), // t5 SUB
    toBin(0x3cf, 12), // t6 SUB
    toBin(0x3f2, 12), // t4 OUT
    toBin(0x3e3, 12), // t5 OUT
    toBin(0x3e3, 12), // t6 OUT
    toBin(0, 12) // Not used
  )

  val t = ringCounter(6, clk, clr)
  val con = rom(controlRomData, presettableCounter(rom(addrRomData, instr), or(t(0), clr), clk, t(2)))

  ControlBus(con, clk, clr)
}

def instrRegister(bus: Bus, load: Port, clk: Port, clr: Port, enable: Port): Spec[Bus] = newSpec {
  val (bus0, bus1) = bus.splitAt(4)
  register(bus0, load, clk) ~> bus0
  register(bus1, load, clk, clr)
}

def progCounter(bus: Bus, count: Port, clk: Port, clr: Port, enable: Port): Spec[Unit] = newSpec {
  buffered(enable)(counter(4, count, clk, clr)) ~> bus.take(4)
}

def inputAndMar(bus: Bus, load: Port, sapIn: Input, clk: Port): Spec[Bus] = newSpec {
  val mOut = register(bus.take(4), load, clk)
  mOut.zip(sapIn.addr).map { case (out, a) => mux(Vector(out, a), Vector(sapIn.prog)) }
}

def ram(bus: Bus, addr: Bus, ce: Port, sapIn: Input): Spec[Unit] = newSpec {
  component.ram(sapIn.data, addr, sapIn.write, and(ce, not(sapIn.prog))) ~> bus
}

def accumulator(bus: Bus, clk: Port, load: Port, enable: Port): Spec[Bus] = newSpec {
  val outs = register(bus, load, clk)
  buffered(enable)(outs) ~> bus
  outs
}

def alu(bus: Bus, ins1: Bus, ins2: Bus, sub: Port, enable: Port): Spec[Unit] = newSpec {
  buffered(enable)(addSub(ins1, ins2, sub)) ~> bus
}
