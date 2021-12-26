package component.sap1

import component.BuilderAPI._
import component._
import core._

val sap1: Spec[Seq[Port]] = newComponent {
  val bus = newPortVec(8)

  val instr = newPortVec(4)
  val ctrl = controller(instr)
  import ctrl._

  instrRegister(bus, li, clk, clr, ei) ~> instr

  progCounter(bus, cp, not(clk), not(clr), ep)

  val (b1, b2) = inputAndMar(bus, lm, clk)
  ram(bus, b1, b2, ce)

  val aOut = accumulator(bus, clk, la, ea)
  val bOut = register(bus, lb, clk)
  alu(bus, aOut, bOut, su, eu)

  register(bus, lo, clk)
}

case class ControlBus(con: Seq[Port], clk: Port, clr: Port) {
  val List(cp, ep, lm, ce, li, ei, la, ea, su, eu, lb, lo) = con.toList
}

def controller(instr: Seq[Port]): Spec[ControlBus] = newComponent {
  ???
}

def instrRegister(bus: Seq[Port], load: Port, clk: Port, clr: Port, enable: Port): Spec[Seq[Port]] = newComponent {
  val (bus0, bus1) = bus.splitAt(4)
  register(bus0, load, clk) ~> bus0
  register(bus1, load, clk, clr)
}

def progCounter(bus: Seq[Port], count: Port, clk: Port, clr: Port, enable: Port): Spec[Unit] = newComponent {
  buffered(enable)(counter(4, count, clk, clr)) ~> bus.take(4)
}

def inputAndMar(bus: Seq[Port], load: Port, clk: Port): Spec[(Seq[Port], Seq[Port])] =
  newComponent {
    ???
  }

def ram(bus: Seq[Port], b1: Seq[Port], b2: Seq[Port], ce: Port): Spec[Unit] = newComponent {
  ???
}

def accumulator(bus: Seq[Port], clk: Port, load: Port, enable: Port): Spec[Seq[Port]] = newComponent {
  val outs = register(bus, load, clk)
  buffered(enable)(outs) ~> bus
  outs
}

def alu(bus: Seq[Port], ins1: Seq[Port], ins2: Seq[Port], sub: Port, enable: Port): Spec[Unit] = newComponent {
  buffered(enable)(addSub(ins1, ins2, sub)) ~> bus
}
