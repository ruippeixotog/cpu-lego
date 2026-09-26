package computer.sap1

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration

import component.BuilderAPI.*
import component.sap1.*
import computer.sap1.Instr.*
import core.*
import simulator.{Circuit, Config, Index, RefSim, Sim, Sync}
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

  private val conf = Config.default
  private val circuit = Circuit(comp)

  // The demo paces the reference simulator well above real time, so that a
  // derived wall-clock phase bound comfortably covers gate propagation.
  private val ticksPerSecond = 100000

  /** Conservative hardware-style phase bound: every combinational path settles within (components + wires + 1)
    * gate/wire delays of its inputs changing, so holding one bound guarantees the previous drive has propagated
    * everywhere. In wall-clock milliseconds, through the simulator's pace. The short-circuit tolerance is excluded:
    * it only governs multi-driver conflicts, not propagation.
    */
  private val phaseBoundMs: Long = {
    val ticks = (circuit.components.size + circuit.wires.size + 1).toLong * math.max(conf.gateDelay, conf.wireDelay)
    (ticks * 1000 + ticksPerSecond - 1) / ticksPerSecond
  }

  /** The RAM's physical instrumentation taps, resolved once from the component index. */
  val ramTaps = RamTaps(
    cells = (0 until 16).map(i => index.buses(s"sap1.ram.ram.cells_$i")).toVector,
    writeGates = index.buses("sap1.ram.ram.writeGates"),
    select = index.buses("sap1.ram.ram.select")
  )

  private val clkOut = index.ports("sap1.clock.out")
  private val ringOut = index.buses("sap1.sequencer.ringCounter.out")

  /** A live simulation of the SAP1. Asserts the asynchronous clear, awaits the reset state on the wires — the derived
    * clock low and the ring counter in its initial state — then releases clear and holds one phase bound so the
    * release propagates everywhere before anything is driven.
    */
  def setup(using ExecutionContext): Future[Sim] = {
    val sim = RefSim(circuit, conf, ticksPerSecond = ticksPerSecond)
    sim.start()
    sim.set(clkSig, false)
    sim.set(clr, false)
    Sync
      .awaitCondition(sim, clkOut +: ringOut)(
        sim.get(clkOut) == Some(false) && sim.get(ringOut) == Vector.fill(6)(Some(false)).updated(0, Some(true))
      )
      .flatMap { _ =>
        sim.set(clr, true)
        Sync.after(phaseBoundMs).map(_ => sim)
      }
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

  /** Load the program and bit-bang the clock until it halts. Returns a Future of the stopped simulator, still readable
    * via get.
    *
    * The clock protocol is observational: each rising edge of the driven clock is followed until the derived clock
    * toggles — or the halt line asserts, which freezes the derived clock — then the driven clock is released for one
    * phase bound, the hardware equivalent of a minimum pulse width. Releasing `prog` after programming likewise holds
    * one bound: a mode switch with no wire to observe.
    */
  def run(using ExecutionContext): Future[Sim] =
    for {
      sim <- setup
      _ <- Programmer.load(sim, ramIn, ramTaps, prog)
      _ <- Sync.after(phaseBoundMs)
      _ <- clockUntilHalt(sim)
    } yield {
      sim.stop()
      sim
    }

  private def clockUntilHalt(sim: Sim)(using ExecutionContext): Future[Unit] = {
    def loop(expectHigh: Boolean): Future[Unit] = {
      sim.set(clkSig, true)
      Sync
        .awaitCondition(sim, Seq(clkOut, hlt))(sim.get(clkOut) == Some(expectHigh) || sim.get(hlt) == Some(true))
        .flatMap { _ =>
          if (debug) printState(sim)
          if (sim.get(hlt) == Some(true)) Future.successful(())
          else {
            sim.set(clkSig, false)
            Sync.after(phaseBoundMs).flatMap(_ => loop(!expectHigh))
          }
        }
    }
    loop(true)
  }
}

object SAP1App extends App {
  given ExecutionContext = ExecutionContext.global

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
  println(Await.result(sap1.run, Duration.Inf).get(sap1.out).render("out: %i"))
}
