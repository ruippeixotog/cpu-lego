package bench

import component.sap1.Input
import computer.sap1.Instr.*
import computer.sap1.{Data, Instr, MemEntry, SAP1}
import core.*
import simulator.{Engine, GateEngine, GateProcessor}
import util.Implicits.*

/** The SAP-1 demo program as a benchmark workload: program the machine once, then run the program to `HLT`, reset, and
  * repeat. One iteration is one full program execution.
  *
  * The driver is engine-native: it mirrors the wall-clock protocol of [[SAP1.run]] — setup,
  * [[computer.sap1.Programmer.load]], clock-until-halt — with tick bounds instead of wall-clock waits, so the measured
  * ticks are pure simulation. RAM has no clear input, so the program survives the per-iteration reset and is loaded
  * only once.
  */
object Sap1Workload extends Workload {

  val name = "sap1"
  val description = "SAP-1 demo program (LDA/ADD/ADD/SUB/OUT/HLT), run to halt, reset, repeat"

  // 10 + 14 + 18 - 20 = 22, the same program SAP1App demos.
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

  def build(): WorkloadRun = new Sap1Run(SAP1(prog))
}

private[bench] final class Sap1Run(sap1: SAP1) extends WorkloadRun {
  import sap1.{circuit, clkSig, clr, conf, hlt, index, out, phaseBoundTicks, ramIn}

  val engine: Engine = GateEngine(GateProcessor.setup(circuit, conf))

  /** The SAP-1's output bus, for functional assertions in tests. */
  val outBus: Bus = out

  private val bound: Long = phaseBoundTicks
  private val clkOut: Port = index.ports("sap1.clock.out")
  private val ringOut: Bus = index.buses("sap1.sequencer.ringCounter.out")
  private val resetRing: Vector[Option[Boolean]] = Vector.fill(6)(Some(false)).updated(0, Some(true))

  /** Step budget for any single wait: generous over the phase bound. Hitting it means the driver protocol diverged from
    * the circuit, which must fail loudly rather than silently benchmark a stuck engine.
    */
  private val stepBudget: Long = 100 * bound

  val ports: Seq[Port] = index.ports.values.toSeq

  reset()
  loadProgram()
  // Prime: run once so that every measured iterate() starts from the identical post-halt state. Without this, the
  // first iteration's reset starts from the post-load state while later ones start post-halt, and the reset takes a
  // slightly different tick count to settle.
  iterate()

  def iterate(): Long = {
    val t0 = engine.tick
    reset()
    clockUntilHalt()
    engine.tick - t0
  }

  /** Assert clear, let the assert propagate everywhere, verify the reset state on the wires, then release clear and
    * hold one bound so the release propagates everywhere before anything is driven.
    *
    * The full-bound drain before releasing is load-bearing: polling only until the ring counter looks right can release
    * clear while the assert is still propagating elsewhere, and the cross-coupled latches then lock in the resulting
    * unknown values permanently.
    */
  private def reset(): Unit = {
    engine.set(clkSig, Some(false))
    engine.set(clr, Some(false))
    engine.runTo(engine.tick + bound)
    require(
      engine.get(clkOut) == Some(false) && engine.get(ringOut) == resetRing,
      "SAP-1 benchmark driver: reset state not reached after a full phase bound"
    )
    engine.set(clr, Some(true))
    engine.runTo(engine.tick + bound)
  }

  /** The engine-native programmer: the same hardware-style write protocol as [[computer.sap1.Programmer.load]], with
    * tick bounds instead of wall-clock waits.
    */
  private def loadProgram(): Unit = {
    val ram: Input = ramIn
    require(Sap1Workload.prog.length <= (1 << ram.addr.length), "program does not fit in RAM")
    Sap1Workload.prog.zipWithIndex.foreach { case (entry, addr) =>
      val data: Seq[Boolean] = entry match {
        case instr: Instr => instr.repr
        case Data(value) => value.toBoolVec(8)
      }
      engine.set(ram.prog, Some(true))
      engine.set(ram.addr, addr.toBoolVec(ram.addr.length))
      engine.set(ram.data, data)
      engine.set(ram.write, Some(false))
      engine.runTo(engine.tick + bound)
      engine.set(ram.write, Some(true))
      engine.runTo(engine.tick + bound)
      engine.set(ram.write, Some(false))
      engine.runTo(engine.tick + bound)
    }
    engine.set(ram.prog, Some(false))
    engine.runTo(engine.tick + bound)
  }

  /** Bit-bang the clock until the program halts. The derived clock is a toggle flip-flop on the driven clock, so each
    * rising edge of the driven clock is followed until the derived clock reaches the expected level — or the halt line
    * asserts, which freezes the derived clock. Releasing the driven clock holds one bound: the hardware equivalent of a
    * minimum pulse width.
    */
  private def clockUntilHalt(): Unit = {
    var expectHigh = true
    var halted = false
    while (!halted) {
      engine.set(clkSig, Some(true))
      var steps = 0L
      while (steps < stepBudget && engine.get(clkOut) != Some(expectHigh) && engine.get(hlt) != Some(true)) {
        engine.step()
        steps += 1
      }
      require(steps < stepBudget, "SAP-1 benchmark driver timed out waiting for the derived clock")
      halted = engine.get(hlt) == Some(true)
      if (!halted) {
        engine.set(clkSig, Some(false))
        engine.runTo(engine.tick + bound)
        expectHigh = !expectHigh
      }
    }
  }
}
