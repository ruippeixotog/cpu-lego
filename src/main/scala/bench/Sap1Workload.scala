package bench

import component.sap1.Input
import computer.sap1.{Data, Instr, SAP1}
import core.*
import simulator.{Circuit, Engine}
import util.Implicits.*

/** The SAP-1 demo program as a benchmark workload: program the machine once, then run the program to `HLT`, reset, and
  * repeat. One iteration is one full program execution.
  *
  * The driver is engine-native: it mirrors the wall-clock protocol of [[SAP1.run]] — setup,
  * [[computer.sap1.Programmer.load]], clock-until-halt — with tick bounds instead of wall-clock waits. RAM has no
  * clear input, so the program survives the per-iteration reset and is loaded only once.
  */
object Sap1Workload extends Workload {

  val name = "sap1"

  def build(engineFactory: Workload.EngineFactory): WorkloadRun =
    new Sap1Run(SAP1(SAP1.demoProg), engineFactory)
}

private[bench] final class Sap1Run(sap1: SAP1, engineFactory: Workload.EngineFactory) extends WorkloadRun {
  import sap1.{clkSig, clr, hlt, index, out, phaseBoundTicks, ramIn}

  val circuit: Circuit = sap1.circuit

  val engine: Engine =
    engineFactory(sap1.circuit, sap1.conf, sap1.index.ports.map(_.swap).toMap)

  /** The SAP-1's output bus, for functional assertions in tests. */
  val outBus: Ports = out.ports

  private val bound: Long = phaseBoundTicks
  private val clkOut: Port = index.ports("sap1.clock.out")
  private val ringOut: Ports = index.buses("sap1.sequencer.ringCounter.out")
  private val resetRing: Vector[Option[Boolean]] = Vector.fill(6)(Some(false)).updated(0, Some(true))

  /** Step budget for any single wait: generous over the phase bound. Hitting it means the driver protocol diverged from
    * the circuit, which must fail loudly rather than silently benchmark a stuck engine.
    */
  private val stepBudget: Long = 100 * bound

  reset()
  loadProgram()

  /** Reset, then run the program to halt. Returns the clock edges driven during the run. */
  def iterate(): Long = {
    reset()
    clockUntilHalt()
  }

  // Assert clear, verify the reset state, release clear, hold one bound.
  // Release clear only after the assert has fully propagated.
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
    SAP1.demoProg.zipWithIndex.foreach { case (entry, addr) =>
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

  /** Bit-bang the clock until the program halts, counting driven clock edges. The derived clock is a toggle
    * flip-flop on the driven clock, so each rising edge of the driven clock is followed until the derived clock
    * reaches the expected level — or the halt line asserts, which freezes the derived clock. Releasing the driven
    * clock holds one bound: the hardware equivalent of a minimum pulse width.
    */
  private def clockUntilHalt(): Long = {
    var edges = 0L
    var expectHigh = true
    var halted = false
    while (!halted) {
      engine.set(clkSig, Some(true))
      edges += 1
      var steps = 0L
      while (steps < stepBudget && engine.get(clkOut) != Some(expectHigh) && engine.get(hlt) != Some(true)) {
        engine.step()
        steps += 1
      }
      require(steps < stepBudget, "SAP-1 benchmark driver timed out waiting for the derived clock")
      halted = engine.get(hlt) == Some(true)
      if (!halted) {
        engine.set(clkSig, Some(false))
        edges += 1
        engine.runTo(engine.tick + bound)
        expectHigh = !expectHigh
      }
    }
    edges
  }
}
