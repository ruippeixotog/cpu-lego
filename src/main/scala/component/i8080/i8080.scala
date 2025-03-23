package component.i8080

import component.BuilderAPI._
import component._
import core._

import ControlBus._
import ControlBus.Bit._

case class Input(d: Bus, ready: Port, hold: Port, int: Port, reset: Port, clk1: Port, clk2: Port)
case class Output(a: Bus, sync: Port, dbin: Port, wait0: Port, wr: Port, hlda: Port, inte: Port)

case class ControlBus(con: Bus) {
  def apply(b: ControlBus.Bit) = con(b.ordinal)
}

object ControlBus {
  enum Bit { case Lo, Lb, Eu, Su, Ea, La, Ei, Li, Ce, Lm, Ep, Cp }

  def fromBits(bits: Bit*): Vector[Boolean] =
    bits.foldLeft(Vector.fill(12)(false)) { (vec, b) => vec.updated(b.ordinal, true) }
}

// http://bitsavers.trailing-edge.com/components/intel/MCS80/98-153B_Intel_8080_Microcomputer_Systems_Users_Manual_197509.pdf
// https://opencores.org/websvn/filedetails?repname=light8080&path=%2Flight8080%2Ftrunk%2Fvhdl%2Flight8080.vhdl
def i8080(in: Input): Spec[Output] = newSpec {
  val bus = newBus(8)
  // val busLow = bus.take(8)

  // val instr = newBus(8)
  // val hlt = andM(instr: _*)
  // val clk = clock(clkSig, clr, hlt)
  // val con = sequencer(instr, clk, clr)

  // instrRegister(busLow, ???, clk, clr, Low) ~> instr

  // // TODO Input ports

  // progCounter(bus, ???, not(clk), clr, ???)

  // memory(bus, clk)

  // val aOut = bidirRegister(busLow, ???, clk, ???)
  // val tmpOut = bidirRegister(busLow, ???, clk, ???)
  // bidirRegister(busLow, ???, clk, ???)
  // bidirRegister(busLow, ???, clk, ???)
  // alu(busLow, aOut, tmpOut, ???, ???)

  // val o1Out = bidirRegister(busLow, ???, clk, Low)
  // val o2Out = bidirRegister(busLow, ???, clk, Low)
  // (hlt, o1Out, o2Out)
  Output(newBus(16), newPort(), newPort(), newPort(), newPort(), newPort(), newPort())
}

// def clock(clkSig: Port, clr: Port, hlt: Port): Spec[Port] = newSpec {
//   jkFlipFlop(not(hlt), not(hlt), clkSig, clr)._1
// }

// def sequencer(instr: Bus, clk: Port, clr: Port): Spec[ControlBus] = newSpec {
//   ???
// }

// def instrRegister(busLow: Bus, load: Port, clk: Port, clr: Port, enable: Port): Spec[Bus] = newSpec {
//   // val (bus0, bus1) = bus.splitAt(4)
//   // buffered(enable)(component.register(bus1, load, clk)) ~> bus1
//   // register(bus0, load, clk, clr)
//   ???
// }

// def progCounter(bus: Bus, count: Port, clk: Port, clr: Port, enable: Port): Spec[Unit] = newSpec {
//   buffered(enable)(counter(16, count, clk, clr)) ~> bus
// }

// def memory(bus: Bus, clk: Port): Spec[Unit] = newSpec {
//   val marOut = component.register(bus, ???, clk)
//   val mdrOut = bidirRegister(bus.take(8), ???, clk, ???)

//   val ram = component.ram(mdrOut, marOut, ???, ???)
// }

def bidirRegister(bus: Bus, load: Port, clk: Port, enable: Port): Spec[Bus] = newSpec {
  val outs = register(bus, load, clk)
  buffered(enable)(outs) ~> bus
  outs
}

def alu(bus: Bus, clk: Port): Spec[Unit] = newSpec {
  val acc = bidirRegister(bus, ???, clk, ???)
  val act = register(acc, ???, clk)
  val tmp = bidirRegister(bus, ???, ???, ???)

  val flags = register(???, ???, clk)

  // alu(act, tmp, carry)
  ???
}
