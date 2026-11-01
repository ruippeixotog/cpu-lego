package component.i8086

import scala.collection.mutable.ListBuffer

import component.*
import component.BuilderAPI.*
import core.*
import org.specs2.execute.Result
import simulator.GateProcessor
import testkit.*
import util.Implicits.*

class I8086Spec extends BaseSpec {

  /** (result, cf, af) for 8086 DAA, following the 8088 rule: the high test uses the original AL, with an AF-dependent
    * threshold (above 0x9F when AF is set, above 0x99 when it is clear).
    */
  private def daaRef(al: Int, af: Boolean, cf: Boolean): (Int, Boolean, Boolean) = {
    var a = al
    var afOut = false
    if (((a & 0xf) > 9) || af) { a = (a + 6) & 0xff; afOut = true }
    val hi = cf || al > (if (af) 0x9f else 0x99)
    if (hi) a = (a + 0x60) & 0xff
    (a, hi, afOut)
  }

  /** (result, cf, af) for 8086 DAS, following the 8088 rule: the high test uses the original AL, with an AF-dependent
    * threshold (above 0x9F when AF is set, above 0x99 when it is clear).
    */
  private def dasRef(al: Int, af: Boolean, cf: Boolean): (Int, Boolean, Boolean) = {
    var a = al
    var afOut = false
    if (((a & 0xf) > 9) || af) { a = (a - 6) & 0xff; afOut = true }
    val hi = cf || al > (if (af) 0x9f else 0x99)
    if (hi) a = (a - 0x60) & 0xff
    (a, hi, afOut)
  }

  /** (al, ah, af, cf) for 8086 AAA, from the Intel pseudocode. */
  private def aaaRef(al: Int, ah: Int, af: Boolean): (Int, Int, Boolean, Boolean) = {
    val adj = ((al & 0xf) > 9) || af
    val alOut = (if (adj) (al + 6) & 0xff else al) & 0x0f
    val ahOut = (if (adj) ah + 1 else ah) & 0xff
    (alOut, ahOut, adj, adj)
  }

  /** (al, ah, af, cf) for 8086 AAS, from the Intel pseudocode. */
  private def aasRef(al: Int, ah: Int, af: Boolean): (Int, Int, Boolean, Boolean) = {
    val adj = ((al & 0xf) > 9) || af
    val alOut = (if (adj) (al - 6) & 0xff else al) & 0x0f
    val ahOut = (if (adj) ah - 1 else ah) & 0xff
    (alOut, ahOut, adj, adj)
  }

  // Hardware vectors from SingleStepTests/8088 27 (DAA) and 2F (DAS), checked
  // against the circuits directly rather than through the Scala refs:
  // (al, af, cf, result, cfOut, afOut). The AF = 1 cases with AL in 0x9A..0x9F
  // are the ones that distinguish the 8088 rule from Intel's pseudocode.
  private val daaVectors: Seq[(Int, Boolean, Boolean, Int, Boolean, Boolean)] = Seq(
    (0xfa, false, false, 0x60, true, true),
    (0x99, false, false, 0x99, false, false),
    (0x99, true, false, 0x9f, false, true),
    (0x9a, true, false, 0xa0, false, true),
    (0x9f, true, false, 0xa5, false, true),
    (0xa0, false, false, 0x00, true, false),
    (0x00, false, false, 0x00, false, false),
    (0xff, false, true, 0x65, true, true)
  )

  private val dasVectors: Seq[(Int, Boolean, Boolean, Int, Boolean, Boolean)] = Seq(
    (0x9d, false, false, 0x37, true, true),
    (0x03, true, false, 0xfd, false, true),
    (0x00, false, false, 0x00, false, false),
    (0xff, false, false, 0x99, true, true),
    (0x10, false, false, 0x10, false, false)
  )

  "A daa" should {
    "match the 8088 rule for all AL values and AF/CF combinations" in {
      val alIns = Vector.fill(8)(new Port)
      val af = new Port
      val cf = new Port
      val ((result, cfOut, afOut), comp) = buildComponent { daa(alIns, af, cf) }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      for (al <- 0 until 256; a <- 0 to 1; c <- 0 to 1) {
        sim = sim.set(alIns, al.toBoolVec(8)).set(af, a == 1).set(cf, c == 1).run()
        val (expResult, expCf, expAf) = daaRef(al, a == 1, c == 1)
        checkBus(sim, result, expResult, s"DAA al=$al af=$a cf=$c", bad)
        checkPort(sim, cfOut, expCf, s"DAA al=$al af=$a cf=$c CF", bad)
        checkPort(sim, afOut, expAf, s"DAA al=$al af=$a cf=$c AF", bad)
      }
      report(bad.toList, 1024)
    }

    "match SingleStepTests/8088 hardware vectors" in {
      val alIns = Vector.fill(8)(new Port)
      val af = new Port
      val cf = new Port
      val ((result, cfOut, afOut), comp) = buildComponent { daa(alIns, af, cf) }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      for ((al, a, c, r, expCf, expAf) <- daaVectors) {
        sim = sim.set(alIns, al.toBoolVec(8)).set(af, a).set(cf, c).run()
        checkBus(sim, result, r, s"DAA al=$al af=$a cf=$c", bad)
        checkPort(sim, cfOut, expCf, s"DAA al=$al af=$a cf=$c CF", bad)
        checkPort(sim, afOut, expAf, s"DAA al=$al af=$a cf=$c AF", bad)
      }
      report(bad.toList, daaVectors.size)
    }
  }

  "A das" should {
    "match the 8088 rule for all AL values and AF/CF combinations" in {
      val alIns = Vector.fill(8)(new Port)
      val af = new Port
      val cf = new Port
      val ((result, cfOut, afOut), comp) = buildComponent { das(alIns, af, cf) }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      for (al <- 0 until 256; a <- 0 to 1; c <- 0 to 1) {
        sim = sim.set(alIns, al.toBoolVec(8)).set(af, a == 1).set(cf, c == 1).run()
        val (expResult, expCf, expAf) = dasRef(al, a == 1, c == 1)
        checkBus(sim, result, expResult, s"DAS al=$al af=$a cf=$c", bad)
        checkPort(sim, cfOut, expCf, s"DAS al=$al af=$a cf=$c CF", bad)
        checkPort(sim, afOut, expAf, s"DAS al=$al af=$a cf=$c AF", bad)
      }
      report(bad.toList, 1024)
    }

    "match SingleStepTests/8088 hardware vectors" in {
      val alIns = Vector.fill(8)(new Port)
      val af = new Port
      val cf = new Port
      val ((result, cfOut, afOut), comp) = buildComponent { das(alIns, af, cf) }
      var sim = GateProcessor.setup(comp)
      val bad = ListBuffer.empty[String]
      for ((al, a, c, r, expCf, expAf) <- dasVectors) {
        sim = sim.set(alIns, al.toBoolVec(8)).set(af, a).set(cf, c).run()
        checkBus(sim, result, r, s"DAS al=$al af=$a cf=$c", bad)
        checkPort(sim, cfOut, expCf, s"DAS al=$al af=$a cf=$c CF", bad)
        checkPort(sim, afOut, expAf, s"DAS al=$al af=$a cf=$c AF", bad)
      }
      report(bad.toList, dasVectors.size)
    }
  }

  private def checkAaaAas(
      op: (Bus, Bus, Port) => Spec[(Bus, Bus, Port, Port)],
      ref: (Int, Int, Boolean) => (Int, Int, Boolean, Boolean),
      name: String
  ): Result = {
    val alIns = Vector.fill(8)(new Port)
    val ahIns = Vector.fill(8)(new Port)
    val af = new Port
    val ((alOut, ahOut, afOut, cfOut), comp) = buildComponent { op(alIns, ahIns, af) }
    var sim = GateProcessor.setup(comp)
    val bad = ListBuffer.empty[String]
    var count = 0
    for (al <- 0 until 256; ah <- 0 until 256; a <- 0 to 1) {
      count += 1
      sim = sim.set(alIns, al.toBoolVec(8)).set(ahIns, ah.toBoolVec(8)).set(af, a == 1).run()
      val (expAl, expAh, expAf, expCf) = ref(al, ah, a == 1)
      checkBus(sim, alOut, expAl, s"$name al=$al ah=$ah af=$a AL", bad)
      checkBus(sim, ahOut, expAh, s"$name al=$al ah=$ah af=$a AH", bad)
      checkPort(sim, afOut, expAf, s"$name al=$al ah=$ah af=$a AF", bad)
      checkPort(sim, cfOut, expCf, s"$name al=$al ah=$ah af=$a CF", bad)
      if (bad.size >= 5) return report(bad.toList, count)
    }
    report(bad.toList, count)
  }

  "An aaa" should {
    "match the Intel pseudocode" in {
      checkAaaAas(aaa, aaaRef, "AAA")
    }
  }

  "An aas" should {
    "match the Intel pseudocode" in {
      checkAaaAas(aas, aasRef, "AAS")
    }
  }
}
