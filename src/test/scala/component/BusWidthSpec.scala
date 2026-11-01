package component

import scala.compiletime.ops.int.*
import scala.compiletime.testing.typeChecks

import component.BuilderAPI.*
import component.sap1.Input
import core.*
import testkit.*

class BusWidthSpec extends BaseSpec {

  private val a, b = Bus.of(8)(Vector.fill(8)(new Port))
  private val nib = Bus.of(4)(Vector.fill(4)(new Port))
  private val sel2 = Bus.of(2)(Vector.fill(2)(new Port))
  private val w = new Port

  "Bus[N]" should {
    "accept matching widths" in {
      typeChecks("buildComponent(andB(a, b))") must beTrue
      typeChecks("buildComponent { val r: Bus[12] = a ++ nib; r }") must beTrue
      typeChecks("buildComponent { val r: Bus[16] = decoder(nib, w); r }") must beTrue
      typeChecks("buildComponent { val r: Bus[3] = priorityEncoder(a)._1; r }") must beTrue
      typeChecks("buildComponent(mux(nib, sel2))") must beTrue
      typeChecks("val (l, h) = a.splitAt(3); val x: Bus[3] = l; val y: Bus[5] = h") must beTrue
      typeChecks("val x: Bus[16] = zeroExtend(a, 16)") must beTrue
      typeChecks("buildComponent(ram(a, nib, w, w))") must beTrue
    }

    "reject mismatched widths at compile time" in {
      typeChecks("buildComponent(andB(a, nib))") must beFalse
      typeChecks("buildComponent(rippleAdder(a, nib))") must beFalse
      typeChecks("buildComponent(binaryAdder(a, nib))") must beFalse
      typeChecks("buildComponent(a ~> nib)") must beFalse
      typeChecks("buildComponent(mux(a, sel2))") must beFalse
      typeChecks("a.take(9)") must beFalse
      typeChecks("zeroExtend(a, 4)") must beFalse
      typeChecks("val x: Bus[16] = a") must beFalse
      typeChecks("Input(w, w, a, a)") must beFalse
      typeChecks("buildComponent(andB(newBus(8), newBus(4)))") must beFalse
      typeChecks("buildComponent(andB(const(8, 1), const(4, 1)))") must beFalse
    }

    "reject unrelated runtime widths and accept the same one" in {
      typeChecks("def f(n: Int, m: Int) = buildComponent(andB(newBus(n), newBus(m)))") must beFalse
      typeChecks("def f(n: Int) = buildComponent(andB(newBus(n), newBus(n)))") must beTrue
    }

    "defer to runtime checks on dynamic widths" in {
      val x = Bus.dynamic(Vector.fill(3)(new Port))
      val y = Bus.dynamic(Vector.fill(4)(new Port))
      buildComponent(andB(x, y)) must throwAn[AssertionError]
      x.take(9) must throwAn[AssertionError]
    }
  }
}
