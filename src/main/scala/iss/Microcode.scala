package iss

import Cpu6502.Step

/** Per-opcode micro-programs for [[Cpu6502]].
  *
  * The core is a table-driven state machine: each documented opcode decodes to the list of micro-steps executed after
  * its fetch cycle (the fetch itself is done by [[Cpu6502.step]]). Every step performs exactly one bus cycle; any
  * register or latch updates ride along on that cycle. This keeps cycle exactness reviewable — each program reads as
  * the documented cycle sequence — and mirrors how the hand-written DSL CPU will be structured.
  *
  * Addressing modes are shared fragments parameterized by what happens on the final data cycle: `commit` for reads
  * (internal updates, no bus access) and `value` for writes. Indexed read modes ((zp,X), (zp),Y, abs,X, abs,Y) need a
  * conditional extra cycle on a page cross: the fixup read is inserted dynamically with [[Cpu6502.prependStep]], since
  * the cross is only known once the index has been added. Indexed writes and read-modify-write always take the extra
  * cycle, so theirs is unconditional.
  *
  * Undocumented opcodes decode to an empty program: each `step` just fetches the next opcode (a single-cycle NOP). They
  * are fully decoded in #58.
  */
private[iss] object Microcode {

  /** The micro-program for `opcode`: steps executed after its fetch cycle. */
  def program(opcode: Int): List[Step] = table(opcode & 0xff)

  // --- read fragments: `commit` runs on the final data cycle ---

  private def readImmediate(commit: Step): List[Step] = List(c => {
    c.dl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff; commit(c)
  })

  private def readZeroPage(commit: Step): List[Step] = List(
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.dl = c.bus.read(c.adl); commit(c) }
  )

  private def readZeroPageX(commit: Step): List[Step] = List(
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.bus.read(c.adl); c.adl = (c.adl + c.x) & 0xff },
    c => { c.dl = c.bus.read(c.adl); commit(c) }
  )

  private def readZeroPageY(commit: Step): List[Step] = List(
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.bus.read(c.adl); c.adl = (c.adl + c.y) & 0xff },
    c => { c.dl = c.bus.read(c.adl); commit(c) }
  )

  private def readAbsolute(commit: Step): List[Step] = List(
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.adh = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.dl = c.bus.read((c.adh << 8) | c.adl); commit(c) }
  )

  private def readAbsoluteX(commit: Step): List[Step] = List(
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.adh = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => {
      val sum = c.adl + c.x
      val lo = sum & 0xff
      c.dl = c.bus.read((c.adh << 8) | lo)
      if (sum > 0xff) {
        c.prependStep(cc => { cc.dl = cc.bus.read((((cc.adh + 1) & 0xff) << 8) | lo); commit(cc) })
      } else {
        commit(c)
      }
    }
  )

  private def readAbsoluteY(commit: Step): List[Step] = List(
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.adh = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => {
      val sum = c.adl + c.y
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
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.bus.read(c.adl); c.adl = (c.adl + c.x) & 0xff },
    c => { c.dl = c.bus.read(c.adl) },
    c => { c.adh = c.bus.read((c.adl + 1) & 0xff) },
    c => { c.dl = c.bus.read((c.adh << 8) | c.dl); commit(c) }
  )

  private def readIndirectIndexed(commit: Step): List[Step] = List(
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
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
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.bus.write(c.adl, value(c)) }
  )

  private def writeZeroPageX(value: Cpu6502 => Int): List[Step] = List(
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.bus.read(c.adl); c.adl = (c.adl + c.x) & 0xff },
    c => { c.bus.write(c.adl, value(c)) }
  )

  private def writeZeroPageY(value: Cpu6502 => Int): List[Step] = List(
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.bus.read(c.adl); c.adl = (c.adl + c.y) & 0xff },
    c => { c.bus.write(c.adl, value(c)) }
  )

  private def writeAbsolute(value: Cpu6502 => Int): List[Step] = List(
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.adh = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.bus.write((c.adh << 8) | c.adl, value(c)) }
  )

  private def writeAbsoluteX(value: Cpu6502 => Int): List[Step] = List(
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.adh = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.bus.read((c.adh << 8) | ((c.adl + c.x) & 0xff)) },
    c => {
      val sum = c.adl + c.x
      c.bus.write((((c.adh + (sum >> 8)) & 0xff) << 8) | (sum & 0xff), value(c))
    }
  )

  private def writeAbsoluteY(value: Cpu6502 => Int): List[Step] = List(
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.adh = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.bus.read((c.adh << 8) | ((c.adl + c.y) & 0xff)) },
    c => {
      val sum = c.adl + c.y
      c.bus.write((((c.adh + (sum >> 8)) & 0xff) << 8) | (sum & 0xff), value(c))
    }
  )

  private def writeIndexedIndirect(value: Cpu6502 => Int): List[Step] = List(
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.bus.read(c.adl); c.adl = (c.adl + c.x) & 0xff },
    c => { c.dl = c.bus.read(c.adl) },
    c => { c.adh = c.bus.read((c.adl + 1) & 0xff) },
    c => { c.bus.write((c.adh << 8) | c.dl, value(c)) }
  )

  private def writeIndirectIndexed(value: Cpu6502 => Int): List[Step] = List(
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
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

  // --- read-modify-write fragments: read, dummy write of the old value, write of the new ---

  private def rmwZeroPage(op: (Cpu6502, Int) => Int): List[Step] = List(
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.dl = c.bus.read(c.adl) },
    c => { c.bus.write(c.adl, c.dl); c.dl = op(c, c.dl) },
    c => { c.bus.write(c.adl, c.dl) }
  )

  private def rmwZeroPageX(op: (Cpu6502, Int) => Int): List[Step] = List(
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.bus.read(c.adl); c.adl = (c.adl + c.x) & 0xff },
    c => { c.dl = c.bus.read(c.adl) },
    c => { c.bus.write(c.adl, c.dl); c.dl = op(c, c.dl) },
    c => { c.bus.write(c.adl, c.dl) }
  )

  private def rmwAbsolute(op: (Cpu6502, Int) => Int): List[Step] = List(
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.adh = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.dl = c.bus.read((c.adh << 8) | c.adl) },
    c => { c.bus.write((c.adh << 8) | c.adl, c.dl); c.dl = op(c, c.dl) },
    c => { c.bus.write((c.adh << 8) | c.adl, c.dl) }
  )

  private def rmwAbsoluteX(op: (Cpu6502, Int) => Int): List[Step] = List(
    c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => { c.adh = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
    c => {
      val sum = c.adl + c.x
      c.bus.read((c.adh << 8) | (sum & 0xff))
      c.adl = sum & 0xff
      c.adh = (c.adh + (sum >> 8)) & 0xff
    },
    c => { c.dl = c.bus.read((c.adh << 8) | c.adl) },
    c => { c.bus.write((c.adh << 8) | c.adl, c.dl); c.dl = op(c, c.dl) },
    c => { c.bus.write((c.adh << 8) | c.adl, c.dl) }
  )

  // --- branches: the taken path inserts its extra cycles dynamically ---

  private def branch(cond: Cpu6502 => Boolean): List[Step] = List(c => {
    val off = c.bus.read(c.pc) & 0xff
    c.pc = (c.pc + 1) & 0xffff
    if (cond(c)) {
      val target = (c.pc + (if (off < 0x80) off else off - 0x100)) & 0xffff
      val page = c.pc & 0xff00
      if (page == (target & 0xff00)) {
        c.prependStep(cc => { cc.bus.read(cc.pc); cc.pc = target })
      } else {
        c.prependStep(cc => {
          cc.bus.read(cc.pc)
          cc.prependStep(ccc => { ccc.bus.read(page | (target & 0xff)); ccc.pc = target })
        })
      }
    }
  })

  // --- stack helpers ---

  private def pushByte(c: Cpu6502, v: Int): Unit = {
    c.bus.write(0x100 | c.s, v & 0xff)
    c.s = (c.s - 1) & 0xff
  }

  /** Pulls a status byte: bit 5 is forced to 1, the B flag (bit 4) is not stored. */
  private def pullStatus(c: Cpu6502, v: Int): Unit = {
    c.p = (v | Cpu6502.FlagU) & ~Cpu6502.FlagB
  }

  // --- ALU operations: each returns the new value and sets the flags ---

  private def aslOp(c: Cpu6502, v: Int): Int = {
    c.setFlag(Cpu6502.FlagC, (v & 0x80) != 0)
    val r = (v << 1) & 0xff
    c.setNZ(r)
    r
  }

  private def lsrOp(c: Cpu6502, v: Int): Int = {
    c.setFlag(Cpu6502.FlagC, (v & 0x01) != 0)
    val r = (v >> 1) & 0xff
    c.setNZ(r)
    r
  }

  private def rolOp(c: Cpu6502, v: Int): Int = {
    val carryIn = if ((c.p & Cpu6502.FlagC) != 0) 1 else 0
    c.setFlag(Cpu6502.FlagC, (v & 0x80) != 0)
    val r = ((v << 1) | carryIn) & 0xff
    c.setNZ(r)
    r
  }

  private def rorOp(c: Cpu6502, v: Int): Int = {
    val carryIn = if ((c.p & Cpu6502.FlagC) != 0) 0x80 else 0
    c.setFlag(Cpu6502.FlagC, (v & 0x01) != 0)
    val r = ((v >> 1) | carryIn) & 0xff
    c.setNZ(r)
    r
  }

  private def incOp(c: Cpu6502, v: Int): Int = {
    val r = (v + 1) & 0xff
    c.setNZ(r)
    r
  }

  private def decOp(c: Cpu6502, v: Int): Int = {
    val r = (v - 1) & 0xff
    c.setNZ(r)
    r
  }

  /** Decimal low-nibble adjust shared by ADC Seqs. 1 and 2. */
  private def decimalAdjustLo(a: Int, m: Int, carryIn: Int): Int = {
    val al = (a & 0x0f) + (m & 0x0f) + carryIn
    if (al >= 0x0a) ((al + 0x06) & 0x0f) + 0x10 else al
  }

  /** ADC core. Binary mode is the plain binary add; decimal mode follows Bruce Clark's 6502.org tutorial
    * (http://www.6502.org/tutorials/decimal_mode.html, Appendix A): the accumulator and carry come from Seq. 1, N and V
    * from the signed Seq. 2, and Z is exactly as in binary mode.
    */
  private def adcCommit(c: Cpu6502): Unit = {
    val a = c.a
    val m = c.dl
    val carryIn = c.p & Cpu6502.FlagC
    if ((c.p & Cpu6502.FlagD) != 0) {
      // Seq. 1: accumulator and carry.
      var t = (a & 0xf0) + (m & 0xf0) + decimalAdjustLo(a, m, carryIn)
      if (t >= 0xa0) t += 0x60
      c.a = t & 0xff
      c.setFlag(Cpu6502.FlagC, t >= 0x100)
      // Seq. 2, in signed arithmetic: N from bit 7, V from the -128..127 range.
      val s = toSigned(a & 0xf0) + toSigned(m & 0xf0) + decimalAdjustLo(a, m, carryIn)
      c.setFlag(Cpu6502.FlagN, ((s & 0xff) & 0x80) != 0)
      c.setFlag(Cpu6502.FlagV, s < -128 || s > 127)
      // Z is as in binary mode.
      c.setFlag(Cpu6502.FlagZ, ((a + m + carryIn) & 0xff) == 0)
    } else {
      val sum = a + m + carryIn
      val r = sum & 0xff
      c.a = r
      c.setFlag(Cpu6502.FlagC, sum > 0xff)
      c.setFlag(Cpu6502.FlagV, (~(a ^ m) & (a ^ r) & 0x80) != 0)
      c.setNZ(r)
    }
  }

  /** SBC core. Decimal mode follows Seq. 3 of Bruce Clark's tutorial (see [[adcCommit]] for the link). */
  private def sbcCommit(c: Cpu6502): Unit = {
    val a = c.a
    val m = c.dl
    val carryIn = c.p & Cpu6502.FlagC
    if ((c.p & Cpu6502.FlagD) != 0) {
      // Seq. 3; all flags are exactly as in binary mode.
      var al = (a & 0x0f) - (m & 0x0f) + carryIn - 1
      if (al < 0) al = ((al - 0x06) & 0x0f) - 0x10
      var t = (a & 0xf0) - (m & 0xf0) + al
      if (t < 0) t -= 0x60
      c.a = t & 0xff
    } else {
      c.a = (a - m + carryIn - 1) & 0xff
    }
    val diff = a - m + carryIn - 1
    val r = diff & 0xff
    c.setFlag(Cpu6502.FlagC, diff >= 0)
    c.setFlag(Cpu6502.FlagV, ((a ^ m) & (a ^ r) & 0x80) != 0)
    c.setNZ(r)
  }

  private def toSigned(v: Int): Int = if (v >= 0x80) v - 0x100 else v

  private def compareWith(reg: Cpu6502 => Int): Step = c => {
    val diff = reg(c) - c.dl
    c.setNZ(diff & 0xff)
    c.setFlag(Cpu6502.FlagC, reg(c) >= c.dl)
  }

  // --- shared commits ---

  private val loadA: Step = c => { c.a = c.dl; c.setNZ(c.a) }
  private val loadX: Step = c => { c.x = c.dl; c.setNZ(c.x) }
  private val loadY: Step = c => { c.y = c.dl; c.setNZ(c.y) }
  private val andA: Step = c => { c.a &= c.dl; c.setNZ(c.a) }
  private val oraA: Step = c => { c.a |= c.dl; c.setNZ(c.a) }
  private val eorA: Step = c => { c.a ^= c.dl; c.setNZ(c.a) }
  private val bitTest: Step = c => {
    c.setFlag(Cpu6502.FlagN, (c.dl & 0x80) != 0)
    c.setFlag(Cpu6502.FlagV, (c.dl & 0x40) != 0)
    c.setFlag(Cpu6502.FlagZ, (c.a & c.dl) == 0)
  }

  // --- the opcode table ---

  private val table: Array[List[Step]] = {
    val t = Array.fill(256)(List.empty[Step])
    def on(opcodes: Int*)(program: List[Step]): Unit = opcodes.foreach(o => t(o & 0xff) = program)

    // LDA
    on(0xa9)(readImmediate(loadA))
    on(0xa5)(readZeroPage(loadA))
    on(0xb5)(readZeroPageX(loadA))
    on(0xad)(readAbsolute(loadA))
    on(0xbd)(readAbsoluteX(loadA))
    on(0xb9)(readAbsoluteY(loadA))
    on(0xa1)(readIndexedIndirect(loadA))
    on(0xb1)(readIndirectIndexed(loadA))

    // LDX
    on(0xa2)(readImmediate(loadX))
    on(0xa6)(readZeroPage(loadX))
    on(0xb6)(readZeroPageY(loadX))
    on(0xae)(readAbsolute(loadX))
    on(0xbe)(readAbsoluteY(loadX))

    // LDY
    on(0xa0)(readImmediate(loadY))
    on(0xa4)(readZeroPage(loadY))
    on(0xb4)(readZeroPageX(loadY))
    on(0xac)(readAbsolute(loadY))
    on(0xbc)(readAbsoluteX(loadY))

    // STA
    on(0x85)(writeZeroPage(c => c.a))
    on(0x95)(writeZeroPageX(c => c.a))
    on(0x8d)(writeAbsolute(c => c.a))
    on(0x9d)(writeAbsoluteX(c => c.a))
    on(0x99)(writeAbsoluteY(c => c.a))
    on(0x81)(writeIndexedIndirect(c => c.a))
    on(0x91)(writeIndirectIndexed(c => c.a))

    // STX
    on(0x86)(writeZeroPage(c => c.x))
    on(0x96)(writeZeroPageY(c => c.x))
    on(0x8e)(writeAbsolute(c => c.x))

    // STY
    on(0x84)(writeZeroPage(c => c.y))
    on(0x94)(writeZeroPageX(c => c.y))
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

    // ADC
    on(0x69)(readImmediate(adcCommit))
    on(0x65)(readZeroPage(adcCommit))
    on(0x75)(readZeroPageX(adcCommit))
    on(0x6d)(readAbsolute(adcCommit))
    on(0x7d)(readAbsoluteX(adcCommit))
    on(0x79)(readAbsoluteY(adcCommit))
    on(0x61)(readIndexedIndirect(adcCommit))
    on(0x71)(readIndirectIndexed(adcCommit))

    // SBC
    on(0xe9)(readImmediate(sbcCommit))
    on(0xe5)(readZeroPage(sbcCommit))
    on(0xf5)(readZeroPageX(sbcCommit))
    on(0xed)(readAbsolute(sbcCommit))
    on(0xfd)(readAbsoluteX(sbcCommit))
    on(0xf9)(readAbsoluteY(sbcCommit))
    on(0xe1)(readIndexedIndirect(sbcCommit))
    on(0xf1)(readIndirectIndexed(sbcCommit))

    // AND
    on(0x29)(readImmediate(andA))
    on(0x25)(readZeroPage(andA))
    on(0x35)(readZeroPageX(andA))
    on(0x2d)(readAbsolute(andA))
    on(0x3d)(readAbsoluteX(andA))
    on(0x39)(readAbsoluteY(andA))
    on(0x21)(readIndexedIndirect(andA))
    on(0x31)(readIndirectIndexed(andA))

    // ORA
    on(0x09)(readImmediate(oraA))
    on(0x05)(readZeroPage(oraA))
    on(0x15)(readZeroPageX(oraA))
    on(0x0d)(readAbsolute(oraA))
    on(0x1d)(readAbsoluteX(oraA))
    on(0x19)(readAbsoluteY(oraA))
    on(0x01)(readIndexedIndirect(oraA))
    on(0x11)(readIndirectIndexed(oraA))

    // EOR
    on(0x49)(readImmediate(eorA))
    on(0x45)(readZeroPage(eorA))
    on(0x55)(readZeroPageX(eorA))
    on(0x4d)(readAbsolute(eorA))
    on(0x5d)(readAbsoluteX(eorA))
    on(0x59)(readAbsoluteY(eorA))
    on(0x41)(readIndexedIndirect(eorA))
    on(0x51)(readIndirectIndexed(eorA))

    // CMP
    on(0xc9)(readImmediate(compareWith(_.a)))
    on(0xc5)(readZeroPage(compareWith(_.a)))
    on(0xd5)(readZeroPageX(compareWith(_.a)))
    on(0xcd)(readAbsolute(compareWith(_.a)))
    on(0xdd)(readAbsoluteX(compareWith(_.a)))
    on(0xd9)(readAbsoluteY(compareWith(_.a)))
    on(0xc1)(readIndexedIndirect(compareWith(_.a)))
    on(0xd1)(readIndirectIndexed(compareWith(_.a)))

    // CPX
    on(0xe0)(readImmediate(compareWith(_.x)))
    on(0xe4)(readZeroPage(compareWith(_.x)))
    on(0xec)(readAbsolute(compareWith(_.x)))

    // CPY
    on(0xc0)(readImmediate(compareWith(_.y)))
    on(0xc4)(readZeroPage(compareWith(_.y)))
    on(0xcc)(readAbsolute(compareWith(_.y)))

    // BIT: N and V from the operand bits, Z from the AND
    on(0x24)(readZeroPage(bitTest))
    on(0x2c)(readAbsolute(bitTest))

    // ASL
    on(0x0a)(implied(c => { c.a = aslOp(c, c.a) }))
    on(0x06)(rmwZeroPage(aslOp))
    on(0x16)(rmwZeroPageX(aslOp))
    on(0x0e)(rmwAbsolute(aslOp))
    on(0x1e)(rmwAbsoluteX(aslOp))

    // LSR
    on(0x4a)(implied(c => { c.a = lsrOp(c, c.a) }))
    on(0x46)(rmwZeroPage(lsrOp))
    on(0x56)(rmwZeroPageX(lsrOp))
    on(0x4e)(rmwAbsolute(lsrOp))
    on(0x5e)(rmwAbsoluteX(lsrOp))

    // ROL
    on(0x2a)(implied(c => { c.a = rolOp(c, c.a) }))
    on(0x26)(rmwZeroPage(rolOp))
    on(0x36)(rmwZeroPageX(rolOp))
    on(0x2e)(rmwAbsolute(rolOp))
    on(0x3e)(rmwAbsoluteX(rolOp))

    // ROR
    on(0x6a)(implied(c => { c.a = rorOp(c, c.a) }))
    on(0x66)(rmwZeroPage(rorOp))
    on(0x76)(rmwZeroPageX(rorOp))
    on(0x6e)(rmwAbsolute(rorOp))
    on(0x7e)(rmwAbsoluteX(rorOp))

    // INC
    on(0xe6)(rmwZeroPage(incOp))
    on(0xf6)(rmwZeroPageX(incOp))
    on(0xee)(rmwAbsolute(incOp))
    on(0xfe)(rmwAbsoluteX(incOp))

    // DEC
    on(0xc6)(rmwZeroPage(decOp))
    on(0xd6)(rmwZeroPageX(decOp))
    on(0xce)(rmwAbsolute(decOp))
    on(0xde)(rmwAbsoluteX(decOp))

    // branches
    on(0x10)(branch(c => (c.p & Cpu6502.FlagN) == 0))
    on(0x30)(branch(c => (c.p & Cpu6502.FlagN) != 0))
    on(0x50)(branch(c => (c.p & Cpu6502.FlagV) == 0))
    on(0x70)(branch(c => (c.p & Cpu6502.FlagV) != 0))
    on(0x90)(branch(c => (c.p & Cpu6502.FlagC) == 0))
    on(0xb0)(branch(c => (c.p & Cpu6502.FlagC) != 0))
    on(0xd0)(branch(c => (c.p & Cpu6502.FlagZ) == 0))
    on(0xf0)(branch(c => (c.p & Cpu6502.FlagZ) != 0))

    // JMP
    on(0x4c)(
      List(
        c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
        c => { c.adh = c.bus.read(c.pc); c.pc = (c.adh << 8) | c.adl }
      )
    )
    // JMP indirect: the high byte wraps within the page (the famous page-boundary bug)
    on(0x6c)(
      List(
        c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
        c => { c.adh = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
        c => { c.dl = c.bus.read((c.adh << 8) | c.adl) },
        c => {
          val hi = c.bus.read((c.adh << 8) | ((c.adl + 1) & 0xff))
          c.pc = (hi << 8) | c.dl
        }
      )
    )

    // JSR: pushes the address of its last byte
    on(0x20)(
      List(
        c => { c.adl = c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
        c => { c.bus.read(0x100 | c.s) },
        c => pushByte(c, c.pc >> 8),
        c => pushByte(c, c.pc),
        c => { c.adh = c.bus.read(c.pc); c.pc = (c.adh << 8) | c.adl }
      )
    )

    // RTS: pulls the return address, then a dummy read before PC+1
    on(0x60)(
      List(
        c => { c.bus.read(c.pc) },
        c => { c.bus.read(0x100 | c.s); c.s = (c.s + 1) & 0xff },
        c => { c.adl = c.bus.read(0x100 | c.s); c.s = (c.s + 1) & 0xff },
        c => { c.adh = c.bus.read(0x100 | c.s) },
        c => {
          val target = (c.adh << 8) | c.adl
          c.bus.read(target)
          c.pc = (target + 1) & 0xffff
        }
      )
    )

    // RTI: pulls the status (bit 5 forced, B not stored) then the return address
    on(0x40)(
      List(
        c => { c.bus.read(c.pc) },
        c => { c.bus.read(0x100 | c.s); c.s = (c.s + 1) & 0xff },
        c => { pullStatus(c, c.bus.read(0x100 | c.s)); c.s = (c.s + 1) & 0xff },
        c => { c.adl = c.bus.read(0x100 | c.s); c.s = (c.s + 1) & 0xff },
        c => { c.adh = c.bus.read(0x100 | c.s); c.pc = (c.adh << 8) | c.adl }
      )
    )

    // BRK: padding-byte read, push PCH/PCL/P (B set), set I, jump through the vector
    on(0x00)(
      List(
        c => { c.bus.read(c.pc); c.pc = (c.pc + 1) & 0xffff },
        c => pushByte(c, c.pc >> 8),
        c => pushByte(c, c.pc),
        c => { pushByte(c, c.p | 0x30); c.setFlag(Cpu6502.FlagI, true) },
        c => { c.adl = c.bus.read(0xfffe) },
        c => { c.adh = c.bus.read(0xffff); c.pc = (c.adh << 8) | c.adl }
      )
    )

    // stack pushes
    on(0x48)(
      List(
        c => { c.bus.read(c.pc) },
        c => pushByte(c, c.a)
      )
    )
    on(0x08)(
      List(
        c => { c.bus.read(c.pc) },
        c => pushByte(c, c.p | 0x30)
      )
    )

    // stack pulls
    on(0x68)(
      List(
        c => { c.bus.read(c.pc) },
        c => { c.bus.read(0x100 | c.s); c.s = (c.s + 1) & 0xff },
        c => { c.a = c.bus.read(0x100 | c.s); c.setNZ(c.a) }
      )
    )
    on(0x28)(
      List(
        c => { c.bus.read(c.pc) },
        c => { c.bus.read(0x100 | c.s); c.s = (c.s + 1) & 0xff },
        c => pullStatus(c, c.bus.read(0x100 | c.s))
      )
    )

    t
  }
}
