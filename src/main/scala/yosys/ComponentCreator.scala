package yosys

import java.nio.file.Path

import scala.collection.mutable

import component.*
import component.BuilderAPI.*
import core.*
import yosys.Design.Direction as DesignDirection

case class ComponentCreator(design: Design) {

  def create(moduleName: String, inPorts: Map[String, Port | Bus]): Spec[Map[String, Port | Bus]] = {
    val env = summon[BuilderEnv]
    inPorts.foreach(env.register(_, _, Some(Direction.Input)))

    val module = design.modules(moduleName)
    val portReg = mutable.Map[Int | String, Port]("0" -> Low, "1" -> High) // .withDefault(_ => new Port())

    inPorts.foreach {
      case (name, p: Port) =>
        // println(s"[in] Connecting $name to ${module.ports(name).bits.head}")
        p ~> portReg.getOrElseUpdate(module.ports(name).bits.head, new Port())
      case (name, p: Bus) =>
        module.ports(name).bits.foreach { bit => p(bit) ~> portReg.getOrElseUpdate(bit, new Port()) }
    }

    val components = module.cells.map { case (compName, cell) =>
      val inPorts = cell.portDirections
        .filter(_._2 != DesignDirection.Output)
        .map { case (name, dir) =>
          name -> (cell.connections(name) match {
            case Vector(bit) => portReg.getOrElseUpdate(bit, new Port())
            case bits => bits.map(portReg)
          })
        }

      val outPorts = createFromType(cell.`type`, cell.parameters, inPorts)

      outPorts.foreach {
        case (name, p: Port) =>
          // println(s"[comp $compName] Connecting $name to ${cell.connections(name).head}")
          p ~> portReg.getOrElseUpdate(cell.connections(name).head, new Port())
        case (name, p: Bus) =>
          cell.connections(name).foreach { bit => p(bit.asInstanceOf[Int]) ~> portReg(bit) }
      }
    }

    val outPorts = module.ports.filter(_._2.direction == DesignDirection.Output).map { case (name, port) =>
      name -> (port.bits match {
        case Vector(bit) => portReg(bit)
        case _ => port.bits.map(portReg)
      })
    }
    outPorts.foreach(env.register(_, _, Some(Direction.Output)))
    outPorts
  }

  def orNot(in1: Port, in2: Port): Spec[Port] = newSpec {
    or(in1, not(in2))
    // nand(not(in1), in2)
  }

  private def createFromType(
      compType: String,
      parameters: Map[String, String],
      inPorts: Map[String, Port | Bus]
  ): Spec[Map[String, Port | Bus]] =
    // https://yosyshq.readthedocs.io/projects/yosys/en/0.32/CHAPTER_CellLib.html
    compType match {
      case "$_AND_" =>
        Map("Y" -> and(inPorts("A").asInstanceOf[Port], inPorts("B").asInstanceOf[Port]))
      case "$_ORNOT_" =>
        Map("Y" -> orNot(inPorts("A").asInstanceOf[Port], inPorts("B").asInstanceOf[Port]))
      case "$_SR_PP_" =>
        Map("Q" -> flipflop(inPorts("S").asInstanceOf[Port], inPorts("R").asInstanceOf[Port])._1)
      case "$_DFF_P_" =>
        Map("Q" -> dLatch(inPorts("D").asInstanceOf[Port], inPorts("C").asInstanceOf[Port])._1)
      case "$_DFFSR_PNN_" =>
        Map(
          "Q" -> dLatch(
            inPorts("D").asInstanceOf[Port],
            inPorts("C").asInstanceOf[Port],
            inPorts("R").asInstanceOf[Port],
            inPorts("S").asInstanceOf[Port]
          )._1
        )
      case "$_MUX_" =>
        Map(
          "Y" -> mux(
            Vector(inPorts("A").asInstanceOf[Port], inPorts("B").asInstanceOf[Port]),
            Vector(inPorts("S").asInstanceOf[Port])
          )
        )
      case "$_NOT_" =>
        Map("Y" -> not(inPorts("A").asInstanceOf[Port]))
      case "$_OR_" =>
        Map("Y" -> or(inPorts("A").asInstanceOf[Port], inPorts("B").asInstanceOf[Port]))
      case "$_XOR_" =>
        Map("Y" -> xor(inPorts("A").asInstanceOf[Port], inPorts("B").asInstanceOf[Port]))
      case m if !m.startsWith("$") =>
        create(m, inPorts)
      case _ =>
        throw new IllegalArgumentException(s"Unsupported component type: $compType")
    }
}

object ComponentCreator {
  def apply(file: Path): ComponentCreator =
    ComponentCreator(Design.fromJsonFile(file))
}
