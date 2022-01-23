package component.ic

import component._
import core._
import org.scalacheck.Prop.forAll
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}
import testkit.BaseSpec
import util.Implicits._

class T7400Spec extends BaseSpec {

  given Arbitrary[Vector[LogicLevel]] = Arbitrary(
    Gen.listOfN(4, summon[Arbitrary[LogicLevel]].arbitrary).map(_.toVector)
  )

  "A 74181" should {

    val logicFun: Vector[(Boolean, Boolean) => Boolean] = Vector(
      (a, _) => !a,
      (a, b) => !(a || b),
      (a, b) => !a && b,
      (_, _) => false,
      (a, b) => !(a && b),
      (_, b) => !b,
      (a, b) => a ^ b,
      (a, b) => a && !b,
      (a, b) => !a || b,
      (a, b) => !(a ^ b),
      (_, b) => b,
      (a, b) => a && b,
      (_, _) => true,
      (a, b) => a || !b,
      (a, b) => a || b,
      (a, _) => a
    )

    val arithFun: Vector[(Int, Int) => Int] = Vector(
      (a, _) => a,
      (a, b) => a | b,
      (a, b) => a | ~b,
      (_, _) => -1,
      (a, b) => a + (a & ~b),
      (a, b) => (a | b) + (a & ~b),
      (a, b) => a - b - 1,
      (a, b) => (a & ~b) - 1,
      (a, b) => a + (a & b),
      (a, b) => a + b,
      (a, b) => (a | ~b) + (a & b),
      (a, b) => (a & b) - 1,
      (a, _) => a + a,
      (a, b) => (a | b) + a,
      (a, b) => (a | ~b) + a,
      (a, _) => a - 1
    )

    "compute an addition with carry correctly" in forAll {
      (s: Vector[LogicLevel], a: Vector[LogicLevel], b: Vector[LogicLevel], cn: LogicLevel, m: LogicLevel) =>
        (a.length == 4 && b.length == 4 && s.length == 4) ==> {
          val (out, sim) = buildAndRun { t74181(s, a, b, cn, m) }

          val sVal = s.toInt
          val aVal = a.map(_.toBool)
          val bVal = b.map(_.toBool)

          val expectedF =
            if (m.toBool) aVal.zip(bVal).map(logicFun(sVal).tupled)
            else (arithFun(sVal)(aVal.toSignedInt, bVal.toSignedInt) + 1 - cn.toInt).toBoolVec(4)

          sim.get(out.f).sequence aka "F" must beSome(expectedF)
          sim.get(out.aEqB) aka "A = B" must beSome(expectedF.forall(identity))
        }
    }.useSeed("", Seed.fromBase64("l_aTQM1oZUdHqnIqHMfupAmf2GPNWY434B36pQMRUeA=").get)

    "be chainable" in forAll {
      (s: Vector[LogicLevel], a1: Vector[LogicLevel], b1: Vector[LogicLevel], cn: LogicLevel, m: LogicLevel) =>
        (a1.length == 4 && b1.length == 4 && s.length == 4) ==> {
          forAll { (a2: Vector[LogicLevel], b2: Vector[LogicLevel]) =>
            val (out, sim) = buildAndRun {
              val out1 = t74181(s, a1, b1, cn, m)
              val out2 = t74181(s, a2, b2, out1.cn4, m)
              T74181Out(out1.f ++ out2.f, and(out1.aEqB, out2.aEqB), null, out2.cn4, null)
            }

            val sVal = s.toInt
            val aVal = (a1 ++ a2).map(_.toBool)
            val bVal = (b1 ++ b2).map(_.toBool)

            val expectedF =
              if (m.toBool) aVal.zip(bVal).map(logicFun(sVal).tupled)
              else (arithFun(sVal)(aVal.toSignedInt, bVal.toSignedInt) + 1 - cn.toInt).toBoolVec(8)

            sim.get(out.f).sequence aka "F" must beSome(expectedF)
            sim.get(out.aEqB) aka "A = B" must beSome(expectedF.forall(identity))
          }
        }
    }
  }
}
