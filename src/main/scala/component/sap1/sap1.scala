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
  val (clr, _) = flipflop(sapIn.clr, not(sapIn.clr))
  val (step, _) = flipflop(sapIn.step, not(sapIn.step))
  val (manual, auto) = flipflop(not(sapIn.auto), sapIn.auto)
  val clk = nand(nand(step, manual), nand(auto, clock(10000)))

  ControlBus(???, clk, clr)
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
