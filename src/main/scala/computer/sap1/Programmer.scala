package computer.sap1

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import component.sap1.*
import core.*
import simulator.*
import util.Implicits.*

case class Data(value: Int)

type MemEntry = Instr | Data

/** Physical instrumentation taps into the SAP-1's RAM, registered by `component.ram` as named ports. The programming
  * protocol observes these instead of sleeping: they are the simulated equivalent of probing the chip with a logic
  * analyzer.
  *
  * @param cells
  *   per-word latch-state buses holding the stored bits
  * @param writeGates
  *   per-word write-enable wires, derived from the write line and the decoder
  * @param select
  *   the decoder's one-hot word-select bus
  */
case class RamTaps(cells: Vector[Bus], writeGates: Bus, select: Bus)

object Programmer {

  /** Load the program into the SAP1's RAM through the live sim, one word at a time, with a hardware-style write
    * protocol per word:
    *
    *   1. Drive `prog`, the address, and the data. The write line stays low, so every write gate stays closed while
    *      the address settles — a changing address can never corrupt a cell.
    *   2. Await the decoder's word-select bus reading exactly one-hot for the target address. A one-hot reading pins
    *      every address bit at its final value, so the decoder cannot transition again afterwards.
    *   3. Assert `write`.
    *   4. Await the target word's latch states matching the requested data.
    *   5. Deassert `write`.
    *   6. Await the target write gate reading low: the write pulse is fully over before the next address change.
    *
    * Every wait is a wire observation composed through [[scala.concurrent.Future]]; there are no sleeps and no
    * simulator internals involved. Returns a Future completing once the whole program is stored and `prog` is
    * released.
    */
  def load(
      sim: Sim,
      ramIn: Input,
      taps: RamTaps,
      prog: List[MemEntry],
      addr: Int = 0
  )(using ExecutionContext): Future[Unit] = {
    require(addr + prog.length <= taps.cells.size, "program does not fit in RAM")
    prog match {
      case entry :: rest =>
        val data = entry match {
          case instr: Instr => instr.repr
          case Data(value) => value.toBoolVec(8)
        }
        sim.set(ramIn.prog, true)
        sim.set(ramIn.addr, addr.toBoolVec(4))
        sim.set(ramIn.data, data)
        sim.set(ramIn.write, false)
        for {
          _ <- Sync.awaitBus(sim, taps.select, Vector.tabulate(taps.select.size)(i => Some(i == addr)))
          _ = sim.set(ramIn.write, true)
          _ <- Sync.awaitBus(sim, taps.cells(addr), data.map(Some(_)).toVector)
          _ = sim.set(ramIn.write, false)
          _ <- Sync.awaitPort(sim, taps.writeGates(addr), Some(false))
          _ <- load(sim, ramIn, taps, rest, addr + 1)
        } yield ()
      case Nil =>
        sim.set(ramIn.prog, false)
        Future.successful(())
    }
  }
}
