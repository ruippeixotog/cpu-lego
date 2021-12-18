package component

import component.BuilderDSL._
import core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import simulator.{Circuit, Sim}

class ArithmeticSpec extends util.BaseSpec {

  "A FullAdder" should {
    "compute a sum with carry correctly" in forAll { (sig1: LogicLevel, sig2: LogicLevel, sig3: LogicLevel) =>
      val ((out, carry), comp) = buildComponent { implicit env => fullAdder(sig1, sig2, sig3) }
      val state = Sim.runComponent(comp)
      
      val expected = List(sig1, sig2, sig3).map(_.toInt).sum

      state.get(out) must beSome(expected % 2 == 1)
      state.get(carry) must beSome(expected / 2 == 1)
    }
  }
}
