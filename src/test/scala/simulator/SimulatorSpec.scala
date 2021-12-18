package simulator

import component.BuilderDSL._
import component._
import core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class SimulatorSpec extends util.BaseSpec {

  case class Instance(comb: List[Port] => Port, f: List[Boolean] => Boolean)
  case class Spec(numInputs: Int, spec: BuilderEnv => Instance)

  val andSpec = Spec(2, implicit env => Instance(ins => and(ins(0), ins(1)), _.reduce(_ && _)))
  val orSpec = Spec(2, implicit env => Instance(ins => or(ins(0), ins(1)), _.reduce(_ || _)))
  val notSpec = Spec(1, implicit env => Instance(ins => component.not(ins.head), !_.head))

  val genSimpleSpec: Gen[Spec] =
    Gen.oneOf(andSpec, orSpec, notSpec)

  def genCompositeSpec(maxDepth: Int): Gen[Spec] = for {
    spec <- genSimpleSpec
    childSpecsFuncs <- Gen.listOfN(spec.numInputs, genSpec(maxDepth - 1))
  } yield {
    Spec(
      childSpecsFuncs.map(_._1).sum,
      { implicit env: BuilderEnv =>
        val Instance(comb0, f0) = spec.spec(env)
        val children = childSpecsFuncs.map { spec => (spec.numInputs, spec.spec(env)) }

        def combine[A](children: List[(Int, List[A] => A)]): List[A] => List[A] = children match {
          case Nil => _ => Nil
          case (n, comb1) :: rest => { ins =>
            val (args, insRest) = ins.splitAt(n)
            comb1(args) :: combine(rest)(insRest)
          }
        }
        Instance(
          comb0.compose(combine(children.map { case (n, inst) => (n, inst.comb) })),
          f0.compose(combine(children.map { case (n, inst) => (n, inst.f) }))
        )
      }
    )
  }

  def genSpec(maxDepth: Int): Gen[Spec] =
    if (maxDepth == 0) genSimpleSpec
    else Gen.oneOf(genSimpleSpec, genCompositeSpec(maxDepth))

  given Arbitrary[Spec] = Arbitrary(genSpec(10))

  "The CircuitSimulator" should {

    "process correctly simultaneous events" in {
      val (out, comp) = buildComponent { implicit env =>
        nand(nand(High, Low), nand(High, Low))
      }
      val state = Sim.runComponent(comp)
      state.get(out) must beSome(false)
    }

    "compute complex boolean expression trees" in forAll { (spec: Spec) =>
      forAll(Gen.listOfN(spec.numInputs, genLogicLevel)) { sigs =>
        val ((out, expected), comp) = buildComponent { env =>
          val Instance(comp, f) = spec.spec(env)
          (comp(sigs), f(sigs.map(_.toBool)))
        }
        val state = Sim.runComponent(comp)
        state.get(out) must beSome(expected)
      }
    }
  }
}
