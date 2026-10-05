package component

import component.BuilderAPI.*
import core.*
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import simulator.GateProcessor
import testkit.*
import util.Implicits.*

class BusSpec extends BaseSpec {

  val genBus16: Gen[Vector[LogicLevel]] = Gen.listOfN(16, genLogicLevel).map(_.toVector)

  // --- helpers ---

  /** All `width`-bit buses, LSB-first. */
  def allBuses(width: Int): Seq[Vector[LogicLevel]] =
    (0 until (1 << width)).map(i => i.toBoolVec(width).map(b => if (b) High else Low).toVector)

  /** All combinations of `n` `width`-bit words. */
  def allWordCombos(n: Int, width: Int): Seq[Vector[Vector[LogicLevel]]] =
    if (n == 0) Seq(Vector())
    else for { w <- allBuses(width); rest <- allWordCombos(n - 1, width) } yield w +: rest

  def bools(bus: Vector[LogicLevel]): Vector[Boolean] = bus.map(_.toBool)

  def constBus(width: Int, value: Int): Vector[LogicLevel] =
    value.toBoolVec(width).map(b => if (b) High else Low).toVector

  /** Reads every simulated bit as a `Vector[Boolean]`, sidestepping the `List`/`Vector` mismatch of `Seq#sequence`.
    */
  def simBools(out: Bus, sim: GateProcessor): Option[Vector[Boolean]] =
    sim.get(out).sequence.map(_.toVector)

  // --- constants and structure (pure helpers: no simulation needed) ---

  "const" should {
    "encode every value exhaustively for width <= 4" in {
      foreach((0 to 4).toList) { width =>
        foreach((0 until (1 << width)).toList) { value =>
          const(width, value) must beEqualTo(constBus(width, value))
        }
      }
      ok
    }

    "throw for an out-of-range width" in {
      const(-1, 0) must throwAn[AssertionError]
      const(64, 0) must throwAn[AssertionError]
    }
  }

  "zeroExtend" should {
    "pad with Low exhaustively for width <= 4" in {
      foreach((0 to 4).toList) { width =>
        foreach((width to 4).toList) { target =>
          foreach(allBuses(width).toList) { in =>
            zeroExtend(in, target) must beEqualTo(in ++ Vector.fill(target - width)(Low))
          }
        }
      }
      ok
    }

    "throw when the target is narrower than the bus" in {
      zeroExtend(Vector(High), 0) must throwAn[AssertionError]
    }
  }

  "signExtend" should {
    "replicate the sign bit exhaustively for width <= 4" in {
      foreach((1 to 4).toList) { width =>
        foreach((width to 4).toList) { target =>
          foreach(allBuses(width).toList) { in =>
            signExtend(in, target) must beEqualTo(in ++ Vector.fill(target - width)(in.last))
          }
        }
      }
      ok
    }

    "throw on an empty bus or a narrower target" in {
      signExtend(Vector(), 1) must throwAn[AssertionError]
      signExtend(Vector(High), 0) must throwAn[AssertionError]
    }
  }

  // --- bitwise ops ---

  "notB" should {
    "negate every bit exhaustively for width <= 4" in {
      foreach((0 to 4).toList) { width =>
        foreach(allBuses(width).toList) { in =>
          val (out, sim) = buildAndRun { notB(in) }
          simBools(out, sim) must beSome(bools(in).map(!_))
        }
      }
      ok
    }

    "match the Scala reference at width 16" in forAll(genBus16) { in =>
      val (out, sim) = buildAndRun { notB(in) }
      simBools(out, sim) must beSome(bools(in).map(!_))
    }
  }

  "andB/orB/xorB on buses" should {
    "match the Scala reference exhaustively for width <= 4" in {
      val ops = Seq[(String, (Bus, Bus) => Spec[Bus], (Boolean, Boolean) => Boolean)](
        ("and", (a, b) => andB(a, b), (x, y) => x && y),
        ("or", (a, b) => orB(a, b), (x, y) => x || y),
        ("xor", (a, b) => xorB(a, b), (x, y) => x ^ y)
      )
      foreach(ops.toList) { case (_, build, ref) =>
        foreach((0 to 4).toList) { width =>
          foreach(allBuses(width).toList) { a =>
            foreach(allBuses(width).toList) { b =>
              val (out, sim) = buildAndRun { build(a, b) }
              simBools(out, sim) must beSome(bools(a).zip(bools(b)).map { case (x, y) => ref(x, y) }.toVector)
            }
          }
        }
      }
      ok
    }

    "match the Scala reference at width 16" in forAll(genBus16, genBus16) { (a, b) =>
      val (andOut, andSim) = buildAndRun { andB(a, b) }
      val (orOut, orSim) = buildAndRun { orB(a, b) }
      val (xorOut, xorSim) = buildAndRun { xorB(a, b) }
      val (ab, bb) = (bools(a), bools(b))
      simBools(andOut, andSim) must beSome(ab.zip(bb).map(_ && _).toVector)
      simBools(orOut, orSim) must beSome(ab.zip(bb).map(_ || _).toVector)
      simBools(xorOut, xorSim) must beSome(ab.zip(bb).map(_ ^ _).toVector)
    }

    "throw on width mismatch" in {
      buildAndRun { andB(Vector(High), Vector()) } must throwAn[AssertionError]
      buildAndRun { orB(Vector(High), Vector()) } must throwAn[AssertionError]
      buildAndRun { xorB(Vector(High), Vector()) } must throwAn[AssertionError]
    }
  }

  "andB/orB/xorB with a single port" should {
    "match the Scala reference exhaustively for width <= 4" in {
      foreach(List(High, Low)) { sel =>
        foreach((0 to 4).toList) { width =>
          foreach(allBuses(width).toList) { in =>
            val (andOut, andSim) = buildAndRun { andB(in, sel) }
            val (orOut, orSim) = buildAndRun { orB(in, sel) }
            val (xorOut, xorSim) = buildAndRun { xorB(in, sel) }
            val (bs, s) = (bools(in), sel.toBool)
            simBools(andOut, andSim) must beSome(bs.map(_ && s))
            simBools(orOut, orSim) must beSome(bs.map(_ || s))
            simBools(xorOut, xorSim) must beSome(bs.map(_ ^ s))
          }
        }
      }
      ok
    }
  }

  // --- reductions ---

  "orR/andR/xorR" should {
    "match the Scala reference exhaustively for width <= 4" in {
      foreach((0 to 4).toList) { width =>
        foreach(allBuses(width).toList) { in =>
          val (orOut, orSim) = buildAndRun { orR(in) }
          val (andOut, andSim) = buildAndRun { andR(in) }
          val (xorOut, xorSim) = buildAndRun { xorR(in) }
          val bs = bools(in)
          orSim.get(orOut) must beSome(bs.foldLeft(false)(_ || _))
          andSim.get(andOut) must beSome(bs.foldLeft(true)(_ && _))
          xorSim.get(xorOut) must beSome(bs.foldLeft(false)(_ ^ _))
        }
      }
      ok
    }

    "match the Scala reference at width 16" in forAll(genBus16) { in =>
      val (orOut, orSim) = buildAndRun { orR(in) }
      val (andOut, andSim) = buildAndRun { andR(in) }
      val (xorOut, xorSim) = buildAndRun { xorR(in) }
      val bs = bools(in)
      orSim.get(orOut) must beSome(bs.contains(true))
      andSim.get(andOut) must beSome(!bs.contains(false))
      xorSim.get(xorOut) must beSome(bs.count(identity) % 2 == 1)
    }
  }

  // --- comparisons ---

  "eqConst" should {
    "match the Scala reference exhaustively for width <= 4" in {
      foreach((0 to 4).toList) { width =>
        foreach(allBuses(width).toList) { in =>
          foreach((0 until (1 << width)).toList) { value =>
            val (out, sim) = buildAndRun { eqConst(in, value) }
            sim.get(out) must beSome(in.toInt == value)
          }
        }
      }
      ok
    }

    "match the Scala reference at width 16" in forAll(genBus16, Gen.choose(0, (1 << 16) - 1)) { (in, value) =>
      val (out, sim) = buildAndRun { eqConst(in, value) }
      sim.get(out) must beSome(in.toInt == value)
    }
  }

  "equality" should {
    "match the Scala reference exhaustively for width <= 4" in {
      foreach((0 to 4).toList) { width =>
        foreach(allBuses(width).toList) { a =>
          foreach(allBuses(width).toList) { b =>
            val (out, sim) = buildAndRun { eqB(a, b) }
            sim.get(out) must beSome(a.toInt == b.toInt)
          }
        }
      }
      ok
    }

    "match the Scala reference at width 16" in forAll(genBus16, genBus16) { (a, b) =>
      val (out, sim) = buildAndRun { eqB(a, b) }
      sim.get(out) must beSome(a.toInt == b.toInt)
    }

    "throw on width mismatch" in {
      buildAndRun { eqB(Vector(High), Vector()) } must throwAn[AssertionError]
    }
  }

  // --- multiplexing ---

  "mux2" should {
    "select a on Low and b on High, exhaustively for width <= 4" in {
      foreach(List(High, Low)) { sel =>
        foreach((0 to 4).toList) { width =>
          foreach(allBuses(width).toList) { a =>
            foreach(allBuses(width).toList) { b =>
              val (out, sim) = buildAndRun { mux2(a, b, sel) }
              simBools(out, sim) must beSome(if (sel.toBool) bools(b) else bools(a))
            }
          }
        }
      }
      ok
    }

    "match the Scala reference at width 16" in forAll(genBus16, genBus16, genLogicLevel) { (a, b, sel) =>
      val (out, sim) = buildAndRun { mux2(a, b, sel) }
      simBools(out, sim) must beSome(if (sel.toBool) bools(b) else bools(a))
    }

    "throw on width mismatch" in {
      buildAndRun { mux2(Vector(High), Vector(), Low) } must throwAn[AssertionError]
    }
  }

  "muxWords" should {
    "select the addressed word, exhaustively over all word combinations" in {
      // selWidth 0..1 (1-2 words) up to width 4, selWidth 2 (4 words) up to width 2:
      // full cartesian product stays tractable
      foreach(List((0, 4), (1, 4), (2, 2))) { case (selWidth, maxWidth) =>
        val words = 1 << selWidth
        foreach((0 to maxWidth).toList) { width =>
          foreach(allWordCombos(words, width).toList) { ws =>
            foreach((0 until words).toList) { s =>
              val sel = constBus(selWidth, s)
              val (out, sim) = buildAndRun { muxWords(ws, sel) }
              simBools(out, sim) must beSome(bools(ws(s)))
            }
          }
        }
      }
      ok
    }

    "select the addressed word at width 4 with distinct words" in {
      // full-width coverage beyond the tractable cartesian product above
      val ws = Vector(0xa, 0x5, 0xf, 0x0).map(constBus(4, _))
      foreach((0 until 4).toList) { s =>
        val (out, sim) = buildAndRun { muxWords(ws, constBus(2, s)) }
        simBools(out, sim) must beSome(bools(ws(s)))
      }
      ok
    }

    "match the Scala reference at width 16" in forAll(
      Gen.listOfN(4, genBus16).map(_.toVector),
      Gen.choose(0, 3)
    ) { (ws, s) =>
      val (out, sim) = buildAndRun { muxWords(ws, constBus(2, s)) }
      simBools(out, sim) must beSome(bools(ws(s)))
    }

    "throw on a bad word count or ragged words" in {
      val w = Vector(High)
      buildAndRun { muxWords(Vector(w, w, w), Vector(Low)) } must throwAn[AssertionError]
      buildAndRun { muxWords(Vector(w, Vector(High, Low)), Vector(Low)) } must throwAn[AssertionError]
      buildAndRun { muxWords(Vector(), Vector()) } must throwAn[AssertionError]
    }
  }

  "oneHotMux" should {
    "OR the selected words, exhaustively over all word combinations" in {
      foreach(List((1, 4), (2, 3), (3, 2))) { case (n, maxWidth) =>
        foreach((0 to maxWidth).toList) { width =>
          foreach(allWordCombos(n, width).toList) { ws =>
            foreach(allBuses(n).toList) { sels =>
              val (out, sim) = buildAndRun { oneHotMux(ws, sels) }
              val expected = (0 until width).map { i =>
                ws.zip(sels).exists { case (w, s) => w(i).toBool && s.toBool }
              }.toVector
              simBools(out, sim) must beSome(expected)
            }
          }
        }
      }
      ok
    }

    "match the Scala reference at width 16" in forAll(
      Gen.listOfN(4, genBus16).map(_.toVector),
      Gen.listOfN(4, genLogicLevel).map(_.toVector)
    ) { (ws, sels) =>
      val (out, sim) = buildAndRun { oneHotMux(ws, sels) }
      val expected = (0 until 16).map { i =>
        ws.zip(sels).exists { case (w, s) => w(i).toBool && s.toBool }
      }.toVector
      simBools(out, sim) must beSome(expected)
    }

    "throw when selects and words disagree" in {
      buildAndRun { oneHotMux(Vector(Vector(High)), Vector()) } must throwAn[AssertionError]
    }
  }

  "priorityEncoder" should {
    "report the highest set bit, exhaustively for width <= 4" in {
      foreach((1 to 4).toList) { width =>
        foreach(allBuses(width).toList) { in =>
          val ((out, valid), sim) = buildAndRun { priorityEncoder(in) }
          val bs = bools(in)
          val expectedIdx = bs.lastIndexOf(true)
          sim.get(valid) must beSome(expectedIdx >= 0)
          val outWidth = 32 - Integer.numberOfLeadingZeros(width - 1)
          out must haveLength(outWidth)
          simBools(out, sim) must beSome(math.max(expectedIdx, 0).toBoolVec(outWidth).toVector)
        }
      }
      ok
    }

    "match the Scala reference at width 16" in forAll(genBus16) { in =>
      val ((out, valid), sim) = buildAndRun { priorityEncoder(in) }
      val bs = bools(in)
      val expectedIdx = bs.lastIndexOf(true)
      sim.get(valid) must beSome(expectedIdx >= 0)
      out must haveLength(4)
      simBools(out, sim) must beSome(math.max(expectedIdx, 0).toBoolVec(4).toVector)
    }

    "throw on an empty bus" in {
      buildAndRun { priorityEncoder(Vector()) } must throwAn[AssertionError]
    }
  }
}
