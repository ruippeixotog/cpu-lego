package computer.sap1

import component.BuilderAPI._
import component.sap1._
import computer.sap1.Instr._
import core._
import simulator.{Index, Sim}
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

  def printBus(sim: Sim, name: String, path: String, fmt: String = "%b"): Unit = {
    println(s"${name}: ${sim.get(index.buses(path)).render(fmt)}")
  }

  def printState(sim: Sim): Unit = {
    val clk = index.ports("sap1.clock.out")
    val tRing = index.buses("sap1.sequencer.ringCounter.out")
    val tState = sim.get(tRing).indexOf(Some(true)) + 1

    val ram = index.buses("sap1.ram.ram.buffered.xs")
    val ramContent = sim.get(ram)
    val ramInstr = " / " + sim.get(ram).sequence.flatMap(Instr.apply).getOrElse("x")

    println(s"---- t=${sim.tick} clk=${sim.get(clk).get} hlt=${sim.get(hlt).get} ----")
    println(s"t: ${tState}")
    printBus(sim, "instr", "sap1.instr")
    printBus(sim, "con", "sap1.sequencer.out_con")
    printBus(sim, "bus", "sap1.bus")
    println()
    printBus(sim, "pc", "sap1.progCounter.counter.out")
    printBus(sim, "ir", "sap1.instrRegister.register.out", "%b (%u)")
    printBus(sim, "ar", "sap1.accumulator.register.out", "%b (%i)")
    printBus(sim, "br", "sap1.register.out", "%b (%i)")
    printBus(sim, "mr", "sap1.inputAndMar.register.out", "%b (%u)")
    println(s"ram: ${ramContent.render}${ramInstr}")
    printBus(sim, "out", "sap1.out2", "%b (%i)")
    println(s"----")
    println()
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
