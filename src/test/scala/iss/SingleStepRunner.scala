package iss

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import ujson.Value
import util.ExternalAssets

/** A bus cycle observed while running a test. */
private[iss] case class BusCycle(addr: Int, value: Int, write: Boolean)

/** A [[Bus6502]] over a 64K array that records every cycle. Unlisted addresses read as zero.
  */
private[iss] class RecordingBus(init: Map[Int, Int]) extends Bus6502 {
  private val mem: Array[Int] = Array.fill(65536)(0)
  init.foreach { case (addr, value) => mem(addr & 0xffff) = value & 0xff }

  private val log = Vector.newBuilder[BusCycle]

  /** The cycles observed so far, in order. */
  def cycles: Vector[BusCycle] = log.result()

  /** Current contents of `addr` (16-bit). */
  def peek(addr: Int): Int = mem(addr & 0xffff)

  def read(addr: Int): Int = {
    val a = addr & 0xffff
    val v = mem(a)
    log += BusCycle(a, v, write = false)
    v
  }

  def write(addr: Int, value: Int): Unit = {
    val a = addr & 0xffff
    val v = value & 0xff
    mem(a) = v
    log += BusCycle(a, v, write = true)
  }
}

/** Runs the SingleStepTests 6502 suites (github.com/SingleStepTests/65x02, directory `6502`) against [[Cpu6502]].
  *
  * For each test the initial registers and RAM are loaded, the instruction's cycles are stepped, and the final
  * registers, RAM and the per-cycle `[address, value, read/write]` list are compared. The first mismatch is reported
  * with the cycle index and both sides.
  *
  * Test data is fetched on first use through [[util.ExternalAssets]] and cached; it is never vendored. By default every
  * 32nd test per opcode runs; set `CPU_LEGO_SINGLESTEP_FULL=1` for the full set.
  */
object SingleStepRunner {

  /** Base URL of the SingleStepTests suites; per-opcode files live under `6502/v1`. */
  val BaseUrl = "https://raw.githubusercontent.com/SingleStepTests/65x02/main"

  /** Opcodes covered by the core: every opcode with a micro-program in [[Microcode]]. */
  val implementedOpcodes: Vector[Int] = (0 until 256).filter(Microcode.program(_).nonEmpty).toVector

  case class Registers(pc: Int, s: Int, a: Int, x: Int, y: Int, p: Int)

  case class TestCase(
      name: String,
      initial: Registers,
      initialRam: Map[Int, Int],
      expected: Registers,
      expectedRam: Map[Int, Int],
      cycles: Vector[BusCycle]
  )

  /** Asset file name for an opcode's suite, namespaced so other suites can reuse [[util.ExternalAssets]]. */
  def assetName(opcode: Int): String = f"6502/v1/$opcode%02x.json"

  /** Loads the (sampled) test cases for `opcode`. Returns [[Left]] with the cause when the asset is unavailable;
    * callers skip with a clear message.
    */
  def load(opcode: Int): Either[String, Vector[TestCase]] = {
    val name = assetName(opcode)
    ExternalAssets.fetch(name, s"$BaseUrl/$name").map { path =>
      val json = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
      sample(parseAll(json))
    }
  }

  /** Runs one test case. Returns [[None]] on a pass, otherwise a description of the first mismatch.
    */
  def run(tc: TestCase): Option[String] = {
    val bus = new RecordingBus(tc.initialRam)
    val cpu = new Cpu6502(bus)
    cpu.pc = tc.initial.pc & 0xffff
    cpu.s = tc.initial.s & 0xff
    cpu.a = tc.initial.a & 0xff
    cpu.x = tc.initial.x & 0xff
    cpu.y = tc.initial.y & 0xff
    cpu.p = (tc.initial.p | Cpu6502.FlagU) & 0xff

    var n = 0
    while (n < tc.cycles.length) {
      cpu.step()
      n += 1
    }

    val got = bus.cycles
    if (!cpu.idle) {
      Some(s"${tc.name}: instruction still running after ${tc.cycles.length} cycles")
    } else {
      firstCycleMismatch(tc, got)
        .orElse(checkRegisters(cpu, tc))
        .orElse(checkRam(bus, tc))
    }
  }

  // --- internals ---

  private val fullRun: Boolean = sys.env.get("CPU_LEGO_SINGLESTEP_FULL").contains("1")

  private def sample(tests: Vector[TestCase]): Vector[TestCase] =
    if (fullRun) tests else tests.zipWithIndex.collect { case (t, i) if i % 32 == 0 => t }

  private def hex2(v: Int): String = f"${v & 0xff}%02x"
  private def hex4(v: Int): String = f"${v & 0xffff}%04x"

  private def describe(c: BusCycle): String =
    s"${if (c.write) "write" else "read"} ${hex4(c.addr)}=${hex2(c.value)}"

  private def firstCycleMismatch(tc: TestCase, got: Vector[BusCycle]): Option[String] = {
    tc.cycles.zip(got).zipWithIndex.collectFirst {
      case ((expected, actual), i) if expected != actual =>
        s"${tc.name}: cycle $i: expected ${describe(expected)}, got ${describe(actual)}"
    }
  }

  private def checkRegisters(cpu: Cpu6502, tc: TestCase): Option[String] = {
    val regs = List(
      "pc" -> (cpu.pc, tc.expected.pc & 0xffff),
      "s" -> (cpu.s, tc.expected.s & 0xff),
      "a" -> (cpu.a, tc.expected.a & 0xff),
      "x" -> (cpu.x, tc.expected.x & 0xff),
      "y" -> (cpu.y, tc.expected.y & 0xff),
      "p" -> (cpu.p, (tc.expected.p | Cpu6502.FlagU) & 0xff)
    )
    regs.collectFirst {
      case (name, (got, want)) if got != want =>
        s"${tc.name}: register $name: expected ${hex2(want)}, got ${hex2(got)}"
    }
  }

  private def checkRam(bus: RecordingBus, tc: TestCase): Option[String] = {
    tc.expectedRam.collectFirst {
      case (addr, want) if bus.peek(addr) != (want & 0xff) =>
        s"${tc.name}: ram ${hex4(addr)}: expected ${hex2(want)}, got ${hex2(bus.peek(addr))}"
    }
  }

  private def parseAll(json: String): Vector[TestCase] =
    ujson.read(json).arr.map(parseTest).toVector

  private def parseTest(v: Value): TestCase = {
    val obj = v.obj
    TestCase(
      name = obj("name").str,
      initial = parseRegisters(obj("initial")),
      initialRam = parseRam(obj("initial")),
      expected = parseRegisters(obj("final")),
      expectedRam = parseRam(obj("final")),
      cycles = obj("cycles").arr.map(parseCycle).toVector
    )
  }

  private def parseRegisters(v: Value): Registers = {
    val obj = v.obj
    def int(key: String): Int = obj(key).num.toInt
    Registers(pc = int("pc"), s = int("s"), a = int("a"), x = int("x"), y = int("y"), p = int("p"))
  }

  private def parseRam(v: Value): Map[Int, Int] =
    v.obj("ram")
      .arr
      .map { entry =>
        val pair = entry.arr
        pair(0).num.toInt -> pair(1).num.toInt
      }
      .toMap

  private def parseCycle(v: Value): BusCycle = {
    val entry = v.arr
    entry(2).str match {
      case "read" => BusCycle(entry(0).num.toInt, entry(1).num.toInt, write = false)
      case "write" => BusCycle(entry(0).num.toInt, entry(1).num.toInt, write = true)
      case rw => throw new IllegalArgumentException(s"bad cycle direction: $rw")
    }
  }

}
