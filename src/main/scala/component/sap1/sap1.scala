package component.sap1

import component.BuilderAPI._
import component._
import core._

case class MemInput(prog: Port, write: Port, addr: Bus, data: Bus)

case class ControlBus(con: Bus, clk: Port, clr: Port) {
  val Vector(cp, ep, lm, ce, li, ei, la, ea, su, eu, lb, lo) = con
}

def sap1(memIn: MemInput): Spec[Bus] = newSpec {
  val bus = newBus(8)

  val instr = newBus(4)
  val ctrl = controller(instr)
  import ctrl._

  instrRegister(bus, li, clk, clr, ei) ~> instr

  progCounter(bus, cp, not(clk), not(clr), ep)

  val mOut = register(bus.take(4), lm, clk)
  ram(bus, mOut, ce, memIn)

  val aOut = accumulator(bus, clk, la, ea)
  val bOut = register(bus, lb, clk)
  alu(bus, aOut, bOut, su, eu)

  register(bus, lo, clk)
}

def controller(instr: Bus): Spec[ControlBus] = newSpec {
  ???
}

def instrRegister(bus: Bus, load: Port, clk: Port, clr: Port, enable: Port): Spec[Bus] = newSpec {
  val (bus0, bus1) = bus.splitAt(4)
  register(bus0, load, clk) ~> bus0
  register(bus1, load, clk, clr)
}

def progCounter(bus: Bus, count: Port, clk: Port, clr: Port, enable: Port): Spec[Unit] = newSpec {
  buffered(enable)(counter(4, count, clk, clr)) ~> bus.take(4)
}

def inputAndMar(bus: Bus, load: Port, memIn: MemInput, clk: Port): Spec[Bus] = newSpec {
  val mOut = register(bus.take(4), load, clk)
  mOut.zip(memIn.addr).map { case (out, a) => mux(Vector(out, a), Vector(memIn.prog)) }
}

def ram(bus: Bus, addr: Bus, ce: Port, memIn: MemInput): Spec[Unit] = newSpec {
  component.ram(memIn.data, addr, memIn.write, and(ce, not(memIn.prog))) ~> bus
}

def accumulator(bus: Bus, clk: Port, load: Port, enable: Port): Spec[Bus] = newSpec {
  val outs = register(bus, load, clk)
  buffered(enable)(outs) ~> bus
  outs
}

def alu(bus: Bus, ins1: Bus, ins2: Bus, sub: Port, enable: Port): Spec[Unit] = newSpec {
  buffered(enable)(addSub(ins1, ins2, sub)) ~> bus
}
