package computer.sap1

import component.BuilderAPI.*
import component.sap1.*
import computer.sap1.Instr.*
import core.*
import simulator.{Circuit, Index, RefSim, Sim}
import util.Formatter
import util.Implicits.*

case class SAP1(prog: List[MemEntry], debug: Boolean = false) {

  val ramIn = {
    val prog, write = newPort()
    val addr = newBus(4)
    val data = newBus(8)
    Input(prog, write, addr, data)
  }

  val clkSig, clr = newPort()
  val ((hlt, out), comp) = buildComponent { sap1(clkSig, clr, ramIn) }

  val index = Index(comp)

  // The demo paces the reference simulator well above real time, so that a
  // short wall-clock settle comfortably covers gate propagation.
  private val ticksPerSecond = 100000
  private val settleMs = 200

  private def settle(): Unit = Thread.sleep(settleMs)

  /** A live simulation of the SAP1: clock held low, clear asserted then released. The simplest peripherals — two wires
    * driven from the outside world.
    */
  def setup: Sim = {
    val sim = RefSim(Circuit(comp), ticksPerSecond = ticksPerSecond)
    sim.start()
    sim.set(clkSig, false)
    sim.set(clr, false)
    settle()
    sim.set(clr, true)
    settle()
    sim
  }

  def printState(sim: Sim): Unit = {
    val fmt = Formatter(sim, index) {
      case ("r", v) => v.indexOf(Some(true)) + 1
      case ("ins", v) => v.sequence.flatMap(Instr.apply).getOrElse("x")
    }

    fmt.print(s"""
      |---- clk=%b{sap1.clock.out} hlt=%b{sap1.out1} ----
      |t: %r{sap1.sequencer.ringCounter.out}
      |instr: %b{sap1.instr}
      |con: %b{sap1.sequencer.out_con[0,4]} %b{*[4,8]} %b{*[8,12]}
      |bus: %b{sap1.bus}
      |
      |pc: %b{sap1.progCounter.counter.out}
      |ir: %b{sap1.instrRegister.register.out} (%u{*})
      |ar: %b{sap1.accumulator.register.out} (%i{*})
      |br: %b{sap1.register.out} (%i{*})
      |mr: %b{sap1.inputAndMar.register.out} (%u{*})
      |ram: %b{sap1.ram.ram.buffered.xs} / %ins{*}
      |out: %b{sap1.out2} (%i{*})
      |----
    """.stripMargin)
  }

  /** Load the program and bit-bang the clock until it halts. Returns the stopped simulator, still readable via get.
    */
  def run: Sim = {
    val sim = setup
    Programmer.load(sim, ramIn, prog)
    while (sim.get(hlt) != Some(true)) {
      if (debug) printState(sim)
      sim.set(clkSig, true)
      settle()
      sim.set(clkSig, false)
      settle()
    }
    if (debug) printState(sim)
    sim.stop()
    sim
  }
}

object SAP1App extends App {

  // 10 + 14 + 18 - 20 = 22
  val prog: List[MemEntry] = List(
    LDA(9),
    ADD(10),
    ADD(11),
    SUB(12),
    OUT,
    HLT,
    Data(0),
    Data(0),
    Data(0),
    Data(10),
    Data(14),
    Data(18),
    Data(20)
  )

  val sap1 = SAP1(prog, debug = true)
  println(sap1.run.get(sap1.out).render("out: %i"))
}
