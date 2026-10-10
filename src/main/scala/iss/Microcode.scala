package iss

import Cpu6502.Step

/** Per-opcode micro-programs for [[Cpu6502]].
  *
  * The core is a table-driven state machine: each documented opcode decodes to the list of micro-steps executed after
  * its fetch cycle (the fetch itself is done by [[Cpu6502.step]]). Every step performs exactly one bus cycle; any
  * register or latch updates ride along on that cycle. This keeps cycle exactness reviewable: each program reads as the
  * documented cycle sequence.
  *
  * Addressing modes are shared fragments parameterized by what happens on the final data cycle: `commit` for reads
  * (internal updates, no bus access) and `value` for writes. Indexed read modes ((zp),Y, abs,X, abs,Y) need a
  * conditional extra cycle on a page cross: the fixup read is inserted dynamically with [[Cpu6502.prependStep]], since
  * the cross is only known once the index has been added. Indexed writes always take the extra cycle, so theirs is
  * unconditional.
  *
  * Undocumented opcodes decode to an empty program: each `step` just fetches the next opcode (a single-cycle NOP).
  */
private[iss] object Microcode {

  /** The micro-program for `opcode`: steps executed after its fetch cycle. */
  def program(opcode: Int): List[Step] = table(opcode & 0xff)

  // --- read fragments: `commit` runs on the final data cycle ---

  /** Fetches the low address byte: reads the next opcode byte into `adl` and advances `pc`. */
  private val fetchAdl: Step = c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff }

  /** Fetches the high address byte: reads the next opcode byte into `adh` and advances `pc`. */
  private val fetchAdh: Step = c => { c.adh = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff }

  private def readImmediate(commit: Step): List[Step] = List(c => {
    c.dl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff; commit(c)
  })

  private def readZeroPage(commit: Step): List[Step] = List(
    fetchAdl,
    c => { c.dl = c.bus.read(c.adl); commit(c) }
  )

  private def readZeroPageIndexed(index: Cpu6502 => Int)(commit: Step): List[Step] = List(
    fetchAdl,
    c => { c.bus.read(c.adl); c.adl = (c.adl + index(c)) & 0xff },
    c => { c.dl = c.bus.read(c.adl); commit(c) }
  )

  private def readAbsolute(commit: Step): List[Step] = List(
    fetchAdl,
    fetchAdh,
    c => { c.dl = c.bus.read((c.adh << 8) | c.adl); commit(c) }
  )

  private def readAbsoluteIndexed(index: Cpu6502 => Int)(commit: Step): List[Step] = List(
    fetchAdl,
    fetchAdh,
    c => {
      val sum = c.adl + index(c)
      val lo = sum & 0xff
      c.dl = c.bus.read((c.adh << 8) | lo)
      if (sum > 0xff) {
        c.prependStep(cc => { cc.dl = cc.bus.read((((cc.adh + 1) & 0xff) << 8) | lo); commit(cc) })
      } else {
        commit(c)
      }
    }
  )

  private def readIndexedIndirect(commit: Step): List[Step] = List(
    fetchAdl,
    c => { c.bus.read(c.adl); c.adl = (c.adl + c.x) & 0xff },
    c => { c.dl = c.bus.read(c.adl) },
    c => { c.adh = c.bus.read((c.adl + 1) & 0xff) },
    c => { c.dl = c.bus.read((c.adh << 8) | c.dl); commit(c) }
  )

  private def readIndirectIndexed(commit: Step): List[Step] = List(
    fetchAdl,
    c => { c.dl = c.bus.read(c.adl) },
    c => { c.adh = c.bus.read((c.adl + 1) & 0xff) },
    c => {
      val sum = c.dl + c.y
      val lo = sum & 0xff
      c.dl = c.bus.read((c.adh << 8) | lo)
      if (sum > 0xff) {
        c.prependStep(cc => { cc.dl = cc.bus.read((((cc.adh + 1) & 0xff) << 8) | lo); commit(cc) })
      } else {
        commit(c)
      }
    }
  )

  // --- write fragments: `value` supplies the byte written ---

  private def writeZeroPage(value: Cpu6502 => Int): List[Step] = List(
    fetchAdl,
    c => { c.bus.write(c.adl, value(c)) }
  )

  private def writeZeroPageIndexed(index: Cpu6502 => Int)(value: Cpu6502 => Int): List[Step] = List(
    fetchAdl,
    c => { c.bus.read(c.adl); c.adl = (c.adl + index(c)) & 0xff },
    c => { c.bus.write(c.adl, value(c)) }
  )

  private def writeAbsolute(value: Cpu6502 => Int): List[Step] = List(
    fetchAdl,
    fetchAdh,
    c => { c.bus.write((c.adh << 8) | c.adl, value(c)) }
  )

  private def writeAbsoluteIndexed(index: Cpu6502 => Int)(value: Cpu6502 => Int): List[Step] = List(
    fetchAdl,
    fetchAdh,
    c => { c.bus.read((c.adh << 8) | ((c.adl + index(c)) & 0xff)) },
    c => {
      val sum = c.adl + index(c)
      c.bus.write((((c.adh + (sum >> 8)) & 0xff) << 8) | (sum & 0xff), value(c))
    }
  )

  private def writeIndexedIndirect(value: Cpu6502 => Int): List[Step] = List(
    fetchAdl,
    c => { c.bus.read(c.adl); c.adl = (c.adl + c.x) & 0xff },
    c => { c.dl = c.bus.read(c.adl) },
    c => { c.adh = c.bus.read((c.adl + 1) & 0xff) },
    c => { c.bus.write((c.adh << 8) | c.dl, value(c)) }
  )

  private def writeIndirectIndexed(value: Cpu6502 => Int): List[Step] = List(
    fetchAdl,
    c => { c.dl = c.bus.read(c.adl) },
    c => { c.adh = c.bus.read((c.adl + 1) & 0xff) },
    c => { c.bus.read((c.adh << 8) | ((c.dl + c.y) & 0xff)) },
    c => {
      val sum = c.dl + c.y
      c.bus.write((((c.adh + (sum >> 8)) & 0xff) << 8) | (sum & 0xff), value(c))
    }
  )

  // --- implied: one dummy read of the next opcode byte, then `commit` ---

  private def implied(commit: Step): List[Step] = List(c => { c.bus.read(c.pc); commit(c) })

  // --- shared commits ---

  private val loadA: Step = c => { c.a = c.dl; c.setNZ(c.a) }
  private val loadX: Step = c => { c.x = c.dl; c.setNZ(c.x) }
  private val loadY: Step = c => { c.y = c.dl; c.setNZ(c.y) }

  // --- the opcode table ---

  private val table: Array[List[Step]] = {
    val t = Array.fill(256)(List.empty[Step])
    def on(opcodes: Int*)(program: List[Step]): Unit = opcodes.foreach(o => t(o & 0xff) = program)

    // LDA
    on(0xa9)(readImmediate(loadA))
    on(0xa5)(readZeroPage(loadA))
    on(0xb5)(readZeroPageIndexed(_.x)(loadA))
    on(0xad)(readAbsolute(loadA))
    on(0xbd)(readAbsoluteIndexed(_.x)(loadA))
    on(0xb9)(readAbsoluteIndexed(_.y)(loadA))
    on(0xa1)(readIndexedIndirect(loadA))
    on(0xb1)(readIndirectIndexed(loadA))

    // LDX
    on(0xa2)(readImmediate(loadX))
    on(0xa6)(readZeroPage(loadX))
    on(0xb6)(readZeroPageIndexed(_.y)(loadX))
    on(0xae)(readAbsolute(loadX))
    on(0xbe)(readAbsoluteIndexed(_.y)(loadX))

    // LDY
    on(0xa0)(readImmediate(loadY))
    on(0xa4)(readZeroPage(loadY))
    on(0xb4)(readZeroPageIndexed(_.x)(loadY))
    on(0xac)(readAbsolute(loadY))
    on(0xbc)(readAbsoluteIndexed(_.x)(loadY))

    // STA
    on(0x85)(writeZeroPage(c => c.a))
    on(0x95)(writeZeroPageIndexed(_.x)(c => c.a))
    on(0x8d)(writeAbsolute(c => c.a))
    on(0x9d)(writeAbsoluteIndexed(_.x)(c => c.a))
    on(0x99)(writeAbsoluteIndexed(_.y)(c => c.a))
    on(0x81)(writeIndexedIndirect(c => c.a))
    on(0x91)(writeIndirectIndexed(c => c.a))

    // STX
    on(0x86)(writeZeroPage(c => c.x))
    on(0x96)(writeZeroPageIndexed(_.y)(c => c.x))
    on(0x8e)(writeAbsolute(c => c.x))

    // STY
    on(0x84)(writeZeroPage(c => c.y))
    on(0x94)(writeZeroPageIndexed(_.x)(c => c.y))
    on(0x8c)(writeAbsolute(c => c.y))

    // transfers
    on(0xaa)(implied(c => { c.x = c.a; c.setNZ(c.x) }))
    on(0xa8)(implied(c => { c.y = c.a; c.setNZ(c.y) }))
    on(0x8a)(implied(c => { c.a = c.x; c.setNZ(c.a) }))
    on(0x98)(implied(c => { c.a = c.y; c.setNZ(c.a) }))
    on(0xba)(implied(c => { c.x = c.s; c.setNZ(c.x) }))
    on(0x9a)(implied(c => { c.s = c.x }))

    // increments (implied)
    on(0xe8)(implied(c => { c.x = (c.x + 1) & 0xff; c.setNZ(c.x) }))
    on(0xc8)(implied(c => { c.y = (c.y + 1) & 0xff; c.setNZ(c.y) }))
    on(0xca)(implied(c => { c.x = (c.x - 1) & 0xff; c.setNZ(c.x) }))
    on(0x88)(implied(c => { c.y = (c.y - 1) & 0xff; c.setNZ(c.y) }))

    // flag operations
    on(0x18)(implied(c => c.setFlag(Cpu6502.FlagC, false)))
    on(0x38)(implied(c => c.setFlag(Cpu6502.FlagC, true)))
    on(0x58)(implied(c => c.setFlag(Cpu6502.FlagI, false)))
    on(0x78)(implied(c => c.setFlag(Cpu6502.FlagI, true)))
    on(0xb8)(implied(c => c.setFlag(Cpu6502.FlagV, false)))
    on(0xd8)(implied(c => c.setFlag(Cpu6502.FlagD, false)))
    on(0xf8)(implied(c => c.setFlag(Cpu6502.FlagD, true)))

    // NOP
    on(0xea)(implied(_ => ()))

    t
  }
}
