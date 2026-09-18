package computer.sap1

import component.sap1.*
import simulator.*
import util.Implicits.*

case class Data(value: Int)

type MemEntry = Instr | Data

object Programmer {

  /** Load the program into the SAP1's RAM through the live sim, pulsing the write line for each entry. Between drive
    * groups, wait for the circuit to settle; the wait is wall-clock time, which is all a live peripheral has.
    */
  def load(sim: Sim, ramIn: Input, prog: List[MemEntry], addr: Int = 0): Unit = {
    prog match {
      case entry :: rest =>
        sim.set(ramIn.prog, true)
        sim.set(ramIn.addr, addr.toBoolVec(4))
        sim.set(
          ramIn.data,
          entry match {
            case instr: Instr => instr.repr
            case Data(value) => value.toBoolVec(8)
          }
        )
        settle()
        sim.set(ramIn.write, true)
        settle()
        sim.set(ramIn.write, false)
        settle()
        load(sim, ramIn, rest, addr + 1)
      case Nil =>
        sim.set(ramIn.prog, false)
        settle()
    }
  }

  private def settle(): Unit = Thread.sleep(200)
}
