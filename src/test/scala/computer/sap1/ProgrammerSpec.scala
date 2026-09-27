package computer.sap1

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

import computer.sap1.Instr.*
import simulator.Sim
import testkit.*
import util.Implicits.*

class ProgrammerSpec extends BaseSpec {
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

  def withLiveSap1[A](prog: List[MemEntry])(f: (SAP1, Sim) => A): A = {
    val sap1 = SAP1(prog)
    val sim = Await.result(sap1.setup, Duration(60, "s"))
    try f(sap1, sim)
    finally sim.stop()
  }

  def runToHalt(sap1: SAP1, sim: Sim): Sim =
    Await.result(sap1.execute(sim), Duration(120, "s"))

  def outValue(sap1: SAP1, sim: Sim): Option[Int] =
    sim.get(sap1.out).sequence.map(_.toSignedInt)

  "Programmer.load" should {

    "leave prog released and the write line low after loading" in withLiveSap1(prog) { (sap1, sim) =>
      Await.result(Programmer.load(sim, sap1.ramIn, prog, sap1.phaseBoundMs), Duration(60, "s"))

      // Black-box: only the peripheral's own input ports are observed, never RAM internals.
      sim.get(sap1.ramIn.prog) must beEqualTo(Some(false))
      sim.get(sap1.ramIn.write) must beEqualTo(Some(false))
    }

    "reject a program that does not fit in RAM" in withLiveSap1(prog) { (sap1, sim) =>
      val tooLong = List.fill(17)(Data(0))
      Await.result(Programmer.load(sim, sap1.ramIn, tooLong, sap1.phaseBoundMs), Duration(60, "s")) must
        throwAn[IllegalArgumentException]
    }
  }

  "A live SAP-1" should {

    "run the demo program to completion" in {
      val sap1 = SAP1(prog)
      val sim = Await.result(sap1.run, Duration(120, "s"))
      sim.isRunning must beFalse
      outValue(sap1, sim) must beEqualTo(Some(22))
    }

    "run the demo program to completion after loading it twice" in withLiveSap1(prog) { (sap1, sim) =>
      // Regression: the tap-based protocol corrupted cells on the second load when the
      // decoder's address transients overlapped the write pulse. The phase-bound protocol
      // holds write low across every address change, so the second load is harmless.
      Await.result(Programmer.load(sim, sap1.ramIn, prog, sap1.phaseBoundMs), Duration(60, "s"))
      Await.result(Programmer.load(sim, sap1.ramIn, prog, sap1.phaseBoundMs), Duration(60, "s"))

      val stopped = runToHalt(sap1, sim)
      stopped.isRunning must beFalse
      outValue(sap1, stopped) must beEqualTo(Some(22))
    }
  }
}
