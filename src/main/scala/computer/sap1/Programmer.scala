package computer.sap1

import scala.concurrent.{ExecutionContext, Future}

import component.sap1.*
import core.*
import simulator.*
import util.Implicits.*

case class Data(value: Int)

type MemEntry = Instr | Data

object Programmer {

  /** Load the program into the SAP1's RAM through the live sim, one word at a time, with a hardware-style write
    * protocol per word:
    *
    *   1. Drive `prog`, the address, and the data, with the write line low.
    *   2. Hold one phase bound: the address and data settle and the decoder's transients — intermediate addresses on
    *      multi-bit changes, overlapping select lines on unequal path delays — die out everywhere before anything is
    *      written. The write line stays low throughout, so a changing address can never corrupt a cell.
    *   3. Assert `write` for one phase bound: the selected cell's write gate opens and its latches capture the data.
    *   4. Deassert `write` and hold one phase bound: the write pulse is fully over — every write gate closed — before
    *      the next address change.
    *
    * Every wait is a wall-clock phase bound composed through [[scala.concurrent.Future]]; there are no wire
    * observations and no simulator internals involved. The bound is conservative: it covers the worst-case
    * combinational settling time of the whole circuit, so no level check can complete before the wires have settled.
    * Returns a Future completing once the whole program is stored and `prog` is released.
    */
  def load(
      sim: Sim,
      ramIn: Input,
      prog: List[MemEntry],
      phaseBoundMs: Long,
      addr: Int = 0
  )(using ExecutionContext): Future[Unit] = {
    require(addr + prog.length <= (1 << ramIn.addr.length), "program does not fit in RAM")
    prog match {
      case entry :: rest =>
        val data = entry match {
          case instr: Instr => instr.repr
          case Data(value) => value.toBoolVec(8)
        }
        sim.set(ramIn.prog, true)
        sim.set(ramIn.addr, addr.toBoolVec(ramIn.addr.length))
        sim.set(ramIn.data, data)
        sim.set(ramIn.write, false)
        for {
          _ <- Sync.after(phaseBoundMs)
          _ = sim.set(ramIn.write, true)
          _ <- Sync.after(phaseBoundMs)
          _ = sim.set(ramIn.write, false)
          _ <- Sync.after(phaseBoundMs)
          _ <- load(sim, ramIn, rest, phaseBoundMs, addr + 1)
        } yield ()
      case Nil =>
        sim.set(ramIn.prog, false)
        Sync.after(phaseBoundMs)
    }
  }
}
