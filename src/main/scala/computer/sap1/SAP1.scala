package computer.sap1

import component.BuilderAPI._
import component.sap1._
import computer.sap1.Instr._
import core._
import simulator.{Index, Sim}
import util.Formatter
import util.Implicits._

case class SAP1(prog: List[MemEntry]) {

  val ramIn = {
    val prog, write = newPort()
    val addr = newBus(4)
    val data = newBus(8)
    Input(prog, write, addr, data)
  }

  val clkSig, clr = newPort()
  val ((hlt, out), comp) = buildComponent { sap1(clkSig, clr, ramIn) }

  val index = Index(comp)

  def setup: Sim =
    Sim.setup(comp).set(clkSig, false).set(clr, false).run().set(clr, true).run()

  def printState(sim: Sim): Unit = {
    val fmt = Formatter(sim, index) {
      case ("r", v) => v.indexOf(Some(true)) + 1
      case ("ins", v) => v.sequence.flatMap(Instr.apply).getOrElse("x")
    }

    fmt.print(s"""
      |---- t=${sim.tick} clk=%b{sap1.clock.out} hlt=%b{sap1.out1} ----
      |t: %r{sap1.sequencer.ringCounter.out}
      |instr: %b{sap1.instr}
      |con: %b{sap1.sequencer.out_con}
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

  def run: Sim = {
    def loop(sim: Sim): Sim = {
      printState(sim)
      if (sim.get(hlt) == Some(true)) sim
      else loop(sim.toggle(clkSig).run().toggle(clkSig).run())
    }
    loop(Programmer.load(setup, ramIn, prog))
  }
}

object SAP1 extends App {

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

  val sap1 = SAP1(prog)
  println(sap1.run.get(sap1.out).render("out: %i"))
}
