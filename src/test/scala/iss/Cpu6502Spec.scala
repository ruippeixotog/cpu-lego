package iss

import org.specs2.mutable.Specification

/** Unit tests for the [[Cpu6502]] core mechanics (one bus cycle per step, instruction boundaries, undocumented-opcode
  * fallback). Behavioral coverage comes from [[SingleStep6502Spec]].
  */
class Cpu6502Spec extends Specification {

  "Cpu6502" should {

    "perform exactly one bus cycle per step" in {
      val bus = new RecordingBus(Map(0x1000 -> 0xa9, 0x1001 -> 0x42))
      val cpu = new Cpu6502(bus)
      cpu.pc = 0x1000

      cpu.step()

      bus.cycles must beEqualTo(Vector(BusCycle(0x1000, 0xa9, write = false)))
      cpu.idle must beFalse
    }

    "execute LDA immediate in two cycles" in {
      val bus = new RecordingBus(Map(0x1000 -> 0xa9, 0x1001 -> 0x42))
      val cpu = new Cpu6502(bus)
      cpu.pc = 0x1000

      cpu.step()
      cpu.step()

      cpu.a must beEqualTo(0x42)
      cpu.pc must beEqualTo(0x1002)
      (cpu.p & Cpu6502.FlagN) must beEqualTo(0)
      (cpu.p & Cpu6502.FlagZ) must beEqualTo(0)
      bus.cycles must beEqualTo(
        Vector(
          BusCycle(0x1000, 0xa9, write = false),
          BusCycle(0x1001, 0x42, write = false)
        )
      )
      cpu.idle must beTrue
    }

    "set N and Z from the loaded value" in {
      val bus = new RecordingBus(Map(0x1000 -> 0xa9, 0x1001 -> 0x00, 0x1002 -> 0xa9, 0x1003 -> 0x80))
      val cpu = new Cpu6502(bus)
      cpu.pc = 0x1000

      cpu.step(); cpu.step()
      (cpu.p & Cpu6502.FlagZ) must beEqualTo(Cpu6502.FlagZ)
      cpu.step(); cpu.step()
      (cpu.p & Cpu6502.FlagN) must beEqualTo(Cpu6502.FlagN)
      (cpu.p & Cpu6502.FlagZ) must beEqualTo(0)
    }

    "keep bit 5 of P hard-wired to 1" in {
      val bus = new RecordingBus(Map(0x1000 -> 0x18)) // CLC
      val cpu = new Cpu6502(bus)
      cpu.pc = 0x1000
      cpu.p = 0x00

      cpu.step(); cpu.step()

      (cpu.p & Cpu6502.FlagU) must beEqualTo(Cpu6502.FlagU)
    }

    "treat undocumented opcodes as single-cycle NOPs until #58" in {
      val bus = new RecordingBus(Map(0x1000 -> 0x02))
      val cpu = new Cpu6502(bus)
      cpu.pc = 0x1000

      cpu.step()

      cpu.pc must beEqualTo(0x1001)
      cpu.idle must beTrue
      bus.cycles must beEqualTo(Vector(BusCycle(0x1000, 0x02, write = false)))
    }

    "leave the interrupt lines unserviced until #57" in {
      val bus = new RecordingBus(Map(0x1000 -> 0xea))
      val cpu = new Cpu6502(bus)
      cpu.pc = 0x1000
      cpu.irq = true
      cpu.nmi = true

      cpu.step(); cpu.step()

      cpu.pc must beEqualTo(0x1001)
      cpu.idle must beTrue
    }
  }
}
