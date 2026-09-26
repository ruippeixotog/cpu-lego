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

  def entryBits(entry: MemEntry): Vector[Boolean] = entry match {
    case instr: Instr => instr.repr
    case Data(value) => value.toBoolVec(8).toVector
  }

  def withLiveSap1[A](prog: List[MemEntry])(f: (SAP1, Sim) => A): A = {
    val sap1 = SAP1(prog)
    val sim = Await.result(sap1.setup, Duration(60, "s"))
    try f(sap1, sim)
    finally sim.stop()
  }

  "The RAM instrumentation" should {

    "expose one cell bus per word plus the write-gate and select buses" in withLiveSap1(prog) { (sap1, _) =>
      sap1.ramTaps.cells must haveLength(16)
      sap1.ramTaps.cells.foreach(_ must haveLength(8))
      sap1.ramTaps.writeGates must haveLength(16)
      sap1.ramTaps.select must haveLength(16)
    }
  }

  "Programmer.load" should {

    "store every word through the live sim, leaving write gates closed" in withLiveSap1(prog) { (sap1, sim) =>
      Await.result(Programmer.load(sim, sap1.ramIn, sap1.ramTaps, prog), Duration(60, "s"))

      prog.zipWithIndex.foreach { case (entry, addr) =>
        sim.get(sap1.ramTaps.cells(addr)).sequence must beEqualTo(Some(entryBits(entry)))
      }
      // Untouched words keep their initial state.
      (prog.length until 16).foreach { addr =>
        sim.get(sap1.ramTaps.cells(addr)).sequence must beNone
      }
      // Every write gate is closed once programming is done.
      sim.get(sap1.ramTaps.writeGates) must beEqualTo(Vector.fill(16)(Some(false)))
    }

    "tolerate loading the identical program twice" in withLiveSap1(prog) { (sap1, sim) =>
      Await.result(Programmer.load(sim, sap1.ramIn, sap1.ramTaps, prog), Duration(60, "s"))
      Await.result(Programmer.load(sim, sap1.ramIn, sap1.ramTaps, prog), Duration(60, "s"))

      prog.zipWithIndex.foreach { case (entry, addr) =>
        sim.get(sap1.ramTaps.cells(addr)).sequence must beEqualTo(Some(entryBits(entry)))
      }
    }

    "reject a program that does not fit in RAM" in withLiveSap1(prog) { (sap1, sim) =>
      val tooLong = List.fill(17)(Data(0))
      Await.result(Programmer.load(sim, sap1.ramIn, sap1.ramTaps, tooLong), Duration(60, "s")) must
        throwAn[IllegalArgumentException]
    }
  }

  "A live SAP-1" should {

    "run the demo program to completion" in {
      val sap1 = SAP1(prog)
      val sim = Await.result(sap1.run, Duration(120, "s"))
      sim.isRunning must beFalse
      sim.get(sap1.out).sequence.map(_.toSignedInt) must beEqualTo(Some(22))
    }
  }
}
