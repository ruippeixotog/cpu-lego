package iss

import Cpu6502.Step

/** A cycle-exact NMOS 6502 instruction-set simulator, in pure Scala.
  *
  * This is the golden model for the hand-written DSL 6502: it reproduces every bus cycle of every documented opcode —
  * including dummy reads and writes — because the Apple II soft switches react to them. It has no dependency on the
  * gate-level simulator; it talks to memory through [[Bus6502]] only.
  *
  * The core is a table-driven state machine (see [[Microcode]]): each documented opcode decodes to the list of
  * micro-steps executed after its fetch cycle, and [[step]] runs exactly one bus cycle. Undocumented opcodes decode to
  * an empty program, i.e. every `step` just fetches the next opcode; they are fully decoded in #58.
  *
  * Interrupts, reset, RDY and SO are wired up in #57; the [[irq]] and [[nmi]] lines are accepted but not serviced yet.
  */
class Cpu6502(val bus: Bus6502) {

  // --- programmer-visible registers ---

  /** Accumulator. */
  var a: Int = 0

  /** X index register. */
  var x: Int = 0

  /** Y index register. */
  var y: Int = 0

  /** Stack pointer: the low byte; the stack lives at `0x0100 | s`. */
  var s: Int = 0

  /** Status register. Bit 5 is hard-wired to 1 on every internal update. */
  var p: Int = 0x24

  /** Program counter. */
  var pc: Int = 0

  // --- interrupt lines (serviced in #57) ---

  /** Interrupt request line (level-sensitive). */
  var irq: Boolean = false

  /** Non-maskable interrupt line (edge-sensitive). */
  var nmi: Boolean = false

  // --- internal latches ---

  private var opcode: Int = 0
  private[iss] var adl: Int = 0
  private[iss] var adh: Int = 0
  private[iss] var dl: Int = 0
  private var pending: List[Step] = Nil

  /** Runs a single bus cycle: one read or one write. */
  def step(): Unit = pending match {
    case Nil =>
      opcode = bus.read(pc) & 0xff
      pc = (pc + 1) & 0xffff
      pending = Microcode.program(opcode)
    case head :: tail =>
      pending = tail
      head(this)
  }

  /** True when no instruction is mid-flight (between the last cycle of one instruction and the fetch of the next).
    */
  private[iss] def idle: Boolean = pending.isEmpty

  /** Inserts a micro-step ahead of the remaining program. Used by indexed addressing modes for their page-cross fixup
    * cycle.
    */
  private[iss] def prependStep(step: Step): Unit = {
    pending = step :: pending
  }

  // --- status flags ---

  /** Sets N and Z from an 8-bit value, preserving the other flags. */
  private[iss] def setNZ(v: Int): Unit = {
    val value = v & 0xff
    p = (p & ~(Cpu6502.FlagN | Cpu6502.FlagZ)) |
      (if (value == 0) Cpu6502.FlagZ else 0) | (value & 0x80) | Cpu6502.FlagU
  }

  /** Sets or clears one status flag, preserving the others. */
  private[iss] def setFlag(flag: Int, on: Boolean): Unit = {
    p = (if (on) p | flag else p & ~flag) | Cpu6502.FlagU
  }
}

object Cpu6502 {

  /** A micro-step: performs exactly one bus cycle plus internal updates. */
  type Step = Cpu6502 => Unit

  // Status flag bits (bit 5, FlagU, is hard-wired to 1).
  val FlagC = 0x01
  val FlagZ = 0x02
  val FlagI = 0x04
  val FlagD = 0x08
  val FlagB = 0x10
  val FlagU = 0x20
  val FlagV = 0x40
  val FlagN = 0x80
}
