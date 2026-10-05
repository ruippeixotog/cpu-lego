package iss

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import util.ExternalAssets

/** Minimal JSON reader for the SingleStepTests schema.
  *
  * Only what the test files need: objects with string keys, arrays, integers and strings. Kept dependency-free on
  * purpose: the files are machine generated with no escapes or floats.
  */
private[iss] object SingleStepJson {

  sealed trait Value
  case class Obj(fields: Map[String, Value]) extends Value
  case class Arr(items: Vector[Value]) extends Value
  case class Str(value: String) extends Value
  case class Num(value: Long) extends Value

  final class ParseException(msg: String) extends RuntimeException(msg)

  def parse(input: String): Value = {
    val p = new Parser(input)
    val v = p.value()
    p.skipWs()
    if (!p.eof) throw new ParseException(s"trailing input at offset ${p.pos}")
    v
  }

  private class Parser(input: String) {
    var pos: Int = 0

    def eof: Boolean = pos >= input.length

    def skipWs(): Unit = {
      while (!eof && input(pos).isWhitespace) pos += 1
    }

    def value(): Value = {
      skipWs()
      if (eof) throw new ParseException("unexpected end of input")
      input(pos) match {
        case '{' => obj()
        case '[' => arr()
        case '"' => Str(str())
        case c if c == '-' || c.isDigit => Num(num())
        case c => throw new ParseException(s"unexpected '$c' at offset $pos")
      }
    }

    private def expect(ch: Char): Unit = {
      skipWs()
      if (eof || input(pos) != ch) throw new ParseException(s"expected '$ch' at offset $pos")
      pos += 1
    }

    private def obj(): Value = {
      expect('{')
      val fields = Map.newBuilder[String, Value]
      skipWs()
      if (!eof && input(pos) == '}') {
        pos += 1
        return Obj(fields.result())
      }
      while (true) {
        skipWs()
        val key = str()
        expect(':')
        fields += key -> value()
        skipWs()
        if (eof) throw new ParseException("unterminated object")
        input(pos) match {
          case ',' => pos += 1
          case '}' => pos += 1; return Obj(fields.result())
          case c => throw new ParseException(s"unexpected '$c' in object at offset $pos")
        }
      }
      throw new AssertionError("unreachable")
    }

    private def arr(): Value = {
      expect('[')
      val items = Vector.newBuilder[Value]
      skipWs()
      if (!eof && input(pos) == ']') {
        pos += 1
        return Arr(items.result())
      }
      while (true) {
        items += value()
        skipWs()
        if (eof) throw new ParseException("unterminated array")
        input(pos) match {
          case ',' => pos += 1
          case ']' => pos += 1; return Arr(items.result())
          case c => throw new ParseException(s"unexpected '$c' in array at offset $pos")
        }
      }
      throw new AssertionError("unreachable")
    }

    private def str(): String = {
      expect('"')
      val sb = new StringBuilder
      while (!eof && input(pos) != '"') {
        if (input(pos) == '\\') {
          pos += 1
          if (eof) throw new ParseException("unterminated escape")
          input(pos) match {
            case '"' => sb += '"'
            case '\\' => sb += '\\'
            case c => throw new ParseException(s"unsupported escape '\\$c' at offset $pos")
          }
          pos += 1
        } else {
          sb += input(pos)
          pos += 1
        }
      }
      expect('"')
      sb.result()
    }

    private def num(): Long = {
      val start = pos
      if (!eof && input(pos) == '-') pos += 1
      while (!eof && input(pos).isDigit) pos += 1
      if (pos == start) throw new ParseException(s"expected number at offset $start")
      input.substring(start, pos).toLong
    }
  }
}

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

  /** Base URL of the SingleStepTests 6502 v1 suites. */
  val BaseUrl = "https://raw.githubusercontent.com/SingleStepTests/65x02/main/6502/v1"

  /** Opcodes covered by the core so far. */
  val implementedOpcodes: Vector[Int] = Vector(
    0xa9, 0xa5, 0xb5, 0xad, 0xbd, 0xb9, 0xa1, 0xb1, // LDA
    0xa2, 0xa6, 0xb6, 0xae, 0xbe, // LDX
    0xa0, 0xa4, 0xb4, 0xac, 0xbc, // LDY
    0x85, 0x95, 0x8d, 0x9d, 0x99, 0x81, 0x91, // STA
    0x86, 0x96, 0x8e, // STX
    0x84, 0x94, 0x8c, // STY
    0xaa, 0xa8, 0x8a, 0x98, 0xba, 0x9a, // transfers
    0xe8, 0xc8, 0xca, 0x88, // increments
    0x18, 0x38, 0x58, 0x78, 0xb8, 0xd8, 0xf8, // flag operations
    0xea // NOP
  )

  case class Registers(pc: Int, s: Int, a: Int, x: Int, y: Int, p: Int)

  case class TestCase(
      name: String,
      initial: Registers,
      initialRam: Map[Int, Int],
      expected: Registers,
      expectedRam: Map[Int, Int],
      cycles: Vector[BusCycle]
  )

  /** Asset file name for an opcode's suite. */
  def assetName(opcode: Int): String = f"$opcode%02x.json"

  /** Loads the (sampled) test cases for `opcode`. Returns [[None]] when the asset is unavailable; callers skip with a
    * clear message.
    */
  def load(opcode: Int): Option[Vector[TestCase]] = {
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
    if (got.length != tc.cycles.length) {
      Some(s"${tc.name}: cycle count ${got.length}, expected ${tc.cycles.length}")
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

  private def parseAll(json: String): Vector[TestCase] = {
    SingleStepJson.parse(json) match {
      case SingleStepJson.Arr(tests) => tests.map(parseTest)
      case _ => throw new SingleStepJson.ParseException("top level must be an array")
    }
  }

  private def parseTest(v: SingleStepJson.Value): TestCase = {
    val obj = asObj(v)
    TestCase(
      name = asStr(obj("name")),
      initial = parseRegisters(asObj(obj("initial"))),
      initialRam = parseRam(asObj(obj("initial"))),
      expected = parseRegisters(asObj(obj("final"))),
      expectedRam = parseRam(asObj(obj("final"))),
      cycles = asArr(obj("cycles")).map(parseCycle).toVector
    )
  }

  private def parseRegisters(obj: Map[String, SingleStepJson.Value]): Registers = {
    def int(key: String): Int = asNum(obj(key)).toInt
    Registers(pc = int("pc"), s = int("s"), a = int("a"), x = int("x"), y = int("y"), p = int("p"))
  }

  private def parseRam(obj: Map[String, SingleStepJson.Value]): Map[Int, Int] = {
    asArr(obj("ram")).map {
      case SingleStepJson.Arr(Vector(SingleStepJson.Num(addr), SingleStepJson.Num(value))) =>
        addr.toInt -> value.toInt
      case other => throw new SingleStepJson.ParseException(s"bad ram entry: $other")
    }.toMap
  }

  private def parseCycle(v: SingleStepJson.Value): BusCycle = {
    v match {
      case SingleStepJson.Arr(Vector(SingleStepJson.Num(addr), SingleStepJson.Num(value), SingleStepJson.Str(rw))) =>
        rw match {
          case "read" => BusCycle(addr.toInt, value.toInt, write = false)
          case "write" => BusCycle(addr.toInt, value.toInt, write = true)
          case _ => throw new SingleStepJson.ParseException(s"bad cycle direction: $rw")
        }
      case other => throw new SingleStepJson.ParseException(s"bad cycle entry: $other")
    }
  }

  private def asObj(v: SingleStepJson.Value): Map[String, SingleStepJson.Value] = v match {
    case SingleStepJson.Obj(fields) => fields
    case _ => throw new SingleStepJson.ParseException(s"expected object, got $v")
  }

  private def asArr(v: SingleStepJson.Value): Vector[SingleStepJson.Value] = v match {
    case SingleStepJson.Arr(items) => items
    case _ => throw new SingleStepJson.ParseException(s"expected array, got $v")
  }

  private def asStr(v: SingleStepJson.Value): String = v match {
    case SingleStepJson.Str(s) => s
    case _ => throw new SingleStepJson.ParseException(s"expected string, got $v")
  }

  private def asNum(v: SingleStepJson.Value): Long = v match {
    case SingleStepJson.Num(n) => n
    case _ => throw new SingleStepJson.ParseException(s"expected number, got $v")
  }

}
