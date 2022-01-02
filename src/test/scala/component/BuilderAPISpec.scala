package component

import component.BuilderAPI._
import core._
import testkit._

class BuilderAPISpec extends BaseSpec {

  def getComponentsFlat(comp: Component, prefix: String = ""): Map[String, Component] = comp match {
    case cc: CompositeComponent =>
      cc.components.foldLeft(Map[String, Component]()) { case (acc, (name, c)) =>
        acc ++ getComponentsFlat(c, prefix + "." + name)
      }
    case c =>
      Map(prefix -> c)
  }

  def getPortsFlat(comp: Component, prefix: String = ""): Map[String, Port | Bus] = comp match {
    case cc: CompositeComponent =>
      cc.namedPorts.map { case (name, p) => (prefix + "." + name, p) } ++
        cc.components.foldLeft(Map[String, Port | Bus]()) { case (acc, (name, c)) =>
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

      getPortsFlat(comp).keySet must beEqualTo(
        Set(
          ".myOperator.myPort",
          ".myOperator.myBus"
        )
      )
    }
  }
}
