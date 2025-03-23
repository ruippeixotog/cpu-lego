package computer.sap1

import component.sap1.*
import simulator.*
import util.Implicits.*

case class Data(value: Int)

type MemEntry = Instr | Data

object Programmer {

  def load(sim: Sim, ramIn: Input, prog: List[MemEntry], addr: Int = 0): Sim = {
    prog match {
      case entry :: rest =>
        val sim1 = sim
          .set(ramIn.prog, true)
          .set(ramIn.addr, addr.toBoolVec(4))
          .set(
            ramIn.data,
            entry match {
              case instr: Instr => instr.repr
              case Data(value) => value.toBoolVec(8)
            }
          )
          .run()
          .set(ramIn.write, true)
          .run()
          .set(ramIn.write, false)
          .run()
        load(sim1, ramIn, rest, addr + 1)
      case Nil =>
        sim.set(ramIn.prog, false).run()
    }
  }
}
