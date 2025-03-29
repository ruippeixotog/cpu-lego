package component

import component.BuilderAPI.*
import core.*
import core.Direction.*
import testkit.*

class BuilderAPISpec extends BaseSpec {

  def getComponentsFlat(comp: Component, prefix: String = ""): Map[String, Component] = comp match {
    case cc: CompositeComponent =>
      cc.components.foldLeft(Map[String, Component]()) { case (acc, (name, c)) =>
        acc ++ getComponentsFlat(c, prefix + "." + name)
      }
    case c =>
      Map(prefix -> c)
  }

  def getPortsFlat(comp: Component, prefix: String = ""): Map[String, (Option[Direction], Port | Bus)] = comp match {
    case cc: CompositeComponent =>
      cc.namedPorts.map { case (name, e) => (prefix + "." + name, e) } ++
        cc.components.foldLeft(Map[String, (Option[Direction], Port | Bus)]()) { case (acc, (name, c)) =>
          acc ++ getPortsFlat(c, prefix + "." + name)
        }
    case _ =>
      Map.empty
  }

  "The BuilderAPI" should {

    "generate named components and ports from newSpec and newPort/newBus" in {
      def myOperator(in: Port): Spec[Port] = newSpec {
        val myPort = newPort()
        val myBus = newBus(4)
        val negate = component.not(in)
        negate
      }
      val (_, comp) = buildComponent {
        val myNand = nand(High, myOperator(High))
        val myNand2 = nand(Low, Low)
        (myNand, myNand2)
      }

      getComponentsFlat(comp).keySet must beEqualTo(
        Set(
          ".nand.impl",
          ".nand$1.impl",
          ".myOperator.not.nand.impl"
        )
      )

      getPortsFlat(comp).map { case (name, (dir, _)) => (name, dir) } must beEqualTo(
        Map(
          ".nand.in1" -> Some(Input),
          ".nand.in2" -> Some(Input),
          ".nand.out" -> Some(Output),
          ".nand$1.in1" -> Some(Input),
          ".nand$1.in2" -> Some(Input),
          ".nand$1.out" -> Some(Output),
          ".myOperator.in" -> Some(Input),
          ".myOperator.myPort" -> None,
          ".myOperator.myBus" -> None,
          ".myOperator.out" -> Some(Output),
          ".myOperator.not.in" -> Some(Input),
          ".myOperator.not.out" -> Some(Output),
          ".myOperator.not.nand.in1" -> Some(Input),
          ".myOperator.not.nand.in2" -> Some(Input),
          ".myOperator.not.nand.out" -> Some(Output)
        )
      )
    }
  }
}
