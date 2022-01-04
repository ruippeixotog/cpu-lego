package simulator

import component.BuilderAPI._
import component._
import core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import testkit._

class SimulatorSpec extends BaseSpec {

  case class Instance(comb: List[Port] => Port, f: List[Boolean] => Boolean)
  class Spec(val numInputs: Int, val instance: BuilderEnv ?=> Instance)

  val andSpec = Spec(2, Instance(ins => and(ins(0), ins(1)), _.reduce(_ && _)))
  val orSpec = Spec(2, Instance(ins => or(ins(0), ins(1)), _.reduce(_ || _)))
  val notSpec = Spec(1, Instance(ins => component.not(ins.head), !_.head))

  val genSimpleSpec: Gen[Spec] =
    Gen.oneOf(andSpec, orSpec, notSpec)

  def genCompositeSpec(maxDepth: Int): Gen[Spec] = for {
    spec <- genSimpleSpec
    childSpecsFuncs <- Gen.listOfN(spec.numInputs, genSpec(maxDepth - 1))
  } yield {
    Spec(
      childSpecsFuncs.map(_.numInputs).sum, {
        val Instance(comb0, f0) = spec.instance
        val children = childSpecsFuncs.map { spec => (spec.numInputs, spec.instance) }

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
      val (out, comp) = buildComponent { nand(nand(High, Low), nand(High, Low)) }
      val state = Sim.runComponent(comp)
      state.get(out) must beSome(false)
    }

    "compute complex boolean expression trees" in forAll { (spec: Spec) =>
      forAll(Gen.listOfN(spec.numInputs, genLogicLevel)) { sigs =>
        val ((out, expected), comp) = buildComponent {
          val Instance(comp, f) = spec.instance
          (comp(sigs), f(sigs.map(_.toBool)))
        }
        val state = Sim.runComponent(comp)
        state.get(out) must beSome(expected)
      }
    }

    "handle well three-state buses" in {
      val in1, load1, in2, load2, bus = newPort()
      val (_, comp) = buildComponent {
        switch(in1, load1) ~> bus
        switch(in2, load2) ~> bus
      }
      runPlan(
        comp,
        100 -> { _.get(bus) must beNone },
        200 -> { _.schedule(0, PortChange(in1, Some(false))).schedule(0, PortChange(in2, Some(true))) },
        300 -> { _.get(bus) must beNone },
        400 -> { _.schedule(0, PortChange(load1, Some(true))) },
        500 -> { _.get(bus) must beSome(false) },
        600 -> { _.schedule(0, PortChange(load1, Some(false))).schedule(0, PortChange(load2, Some(true))) },
        700 -> { _.get(bus) must beSome(true) }
      )
    }
  }
}
