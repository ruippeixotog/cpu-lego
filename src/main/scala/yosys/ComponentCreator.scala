package yosys

import java.nio.file.{Files, Path}

import scala.collection.mutable
import scala.sys.process.*

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
        module.ports(name).bits.zipWithIndex.foreach { (bit, i) => p(i) ~> portReg.getOrElseUpdate(bit, new Port()) }
    }

    val components = module.cells.map { case (compName, cell) =>
      val inPorts = cell.portDirections
        .filter(_._2 != DesignDirection.Output)
        .map { case (name, dir) =>
          name -> (cell.connections(name) match {
            case Vector(bit) => portReg.getOrElseUpdate(bit, new Port())
            case bits => bits.map { b => portReg.getOrElseUpdate(b, new Port()) }
          })
        }

      val outPorts = createFromType(compName, cell, inPorts)

      outPorts.foreach {
        case (name, p: Port) =>
          // println(s"[comp $compName] Connecting $name to ${cell.connections(name).head}")
          p ~> portReg.getOrElseUpdate(cell.connections(name).head, new Port())
        case (name, p: Bus) =>
          cell.connections(name).zipWithIndex.foreach { (bit, i) => p(i) ~> portReg.getOrElseUpdate(bit, new Port()) }
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

  def andNot(in1: Port, in2: Port): Spec[Port] = newSpec {
    and(in1, not(in2))
  }

  private def inPort(inPorts: Map[String, Port | Bus], name: String): Port =
    inPorts(name).asInstanceOf[Port]

  private def createFromType(
      compName: String,
      cell: Design.Cell,
      inPorts: Map[String, Port | Bus]
  ): Spec[Map[String, Port | Bus]] =
    // https://yosyshq.readthedocs.io/projects/yosys/en/0.32/CHAPTER_CellLib.html
    // Semantics below transcribed from techlibs/common/simcells.v.
    cell.`type` match {
      case "$_AND_" =>
        Map("Y" -> and(inPort(inPorts, "A"), inPort(inPorts, "B")))
      case "$_ANDNOT_" =>
        Map("Y" -> andNot(inPort(inPorts, "A"), inPort(inPorts, "B")))
      case "$_AOI3_" =>
        Map("Y" -> not(or(and(inPort(inPorts, "A"), inPort(inPorts, "B")), inPort(inPorts, "C"))))
      case "$_AOI4_" =>
        Map(
          "Y" -> not(
            or(and(inPort(inPorts, "A"), inPort(inPorts, "B")), and(inPort(inPorts, "C"), inPort(inPorts, "D")))
          )
        )
      case "$_BUF_" =>
        Map("Y" -> inPort(inPorts, "A"))
      case "$_MUX_" =>
        Map("Y" -> mux(Vector(inPort(inPorts, "A"), inPort(inPorts, "B")), Vector(inPort(inPorts, "S"))))
      case "$_MUX4_" =>
        Map(
          "Y" -> mux(
            Vector(inPort(inPorts, "A"), inPort(inPorts, "B"), inPort(inPorts, "C"), inPort(inPorts, "D")),
            Vector(inPort(inPorts, "S"), inPort(inPorts, "T"))
          )
        )
      case "$_MUX8_" =>
        Map(
          "Y" -> mux(
            Vector(
              inPort(inPorts, "A"),
              inPort(inPorts, "B"),
              inPort(inPorts, "C"),
              inPort(inPorts, "D"),
              inPort(inPorts, "E"),
              inPort(inPorts, "F"),
              inPort(inPorts, "G"),
              inPort(inPorts, "H")
            ),
            Vector(inPort(inPorts, "S"), inPort(inPorts, "T"), inPort(inPorts, "U"))
          )
        )
      case "$_MUX16_" =>
        Map(
          "Y" -> mux(
            Vector(
              inPort(inPorts, "A"),
              inPort(inPorts, "B"),
              inPort(inPorts, "C"),
              inPort(inPorts, "D"),
              inPort(inPorts, "E"),
              inPort(inPorts, "F"),
              inPort(inPorts, "G"),
              inPort(inPorts, "H"),
              inPort(inPorts, "I"),
              inPort(inPorts, "J"),
              inPort(inPorts, "K"),
              inPort(inPorts, "L"),
              inPort(inPorts, "M"),
              inPort(inPorts, "N"),
              inPort(inPorts, "O"),
              inPort(inPorts, "P")
            ),
            Vector(inPort(inPorts, "S"), inPort(inPorts, "T"), inPort(inPorts, "U"), inPort(inPorts, "V"))
          )
        )
      case "$_NAND_" =>
        Map("Y" -> nand(inPort(inPorts, "A"), inPort(inPorts, "B")))
      case "$_NMUX_" =>
        Map("Y" -> not(mux(Vector(inPort(inPorts, "A"), inPort(inPorts, "B")), Vector(inPort(inPorts, "S")))))
      case "$_NOR_" =>
        Map("Y" -> nor(inPort(inPorts, "A"), inPort(inPorts, "B")))
      case "$_NOT_" =>
        Map("Y" -> not(inPort(inPorts, "A")))
      case "$_OAI3_" =>
        Map("Y" -> not(and(or(inPort(inPorts, "A"), inPort(inPorts, "B")), inPort(inPorts, "C"))))
      case "$_OAI4_" =>
        Map(
          "Y" -> not(
            and(or(inPort(inPorts, "A"), inPort(inPorts, "B")), or(inPort(inPorts, "C"), inPort(inPorts, "D")))
          )
        )
      case "$_OR_" =>
        Map("Y" -> or(inPort(inPorts, "A"), inPort(inPorts, "B")))
      case "$_ORNOT_" =>
        Map("Y" -> orNot(inPort(inPorts, "A"), inPort(inPorts, "B")))
      case "$_SR_PP_" =>
        Map("Q" -> norLatch(inPort(inPorts, "S"), inPort(inPorts, "R"))._1)
      case "$_DFF_P_" =>
        Map("Q" -> dLatch(inPort(inPorts, "D"), inPort(inPorts, "C"))._1)
      case "$_DFFSR_PNN_" =>
        Map(
          "Q" -> dLatch(
            inPort(inPorts, "D"),
            inPort(inPorts, "C"),
            inPort(inPorts, "R"),
            inPort(inPorts, "S")
          )._1
        )
      case "$_XNOR_" =>
        Map("Y" -> xnor(inPort(inPorts, "A"), inPort(inPorts, "B")))
      case "$_XOR_" =>
        Map("Y" -> xor(inPort(inPorts, "A"), inPort(inPorts, "B")))
      case m if !m.startsWith("$") =>
        create(m, inPorts)
      case _ =>
        val src = cell.attributes.get("src").map(s => s", src $s").getOrElse("")
        throw new IllegalArgumentException(s"Unsupported component type: ${cell.`type`} (cell $compName$src)")
    }
}

object ComponentCreator {

  def fromVerilog(vFile: Path): ComponentCreator = {
    val tmpDir = Files.createTempDirectory("cpu-lego")
    val jsonFile = tmpDir.resolve("out.json")

    Files.writeString(
      tmpDir.resolve("script.ys"),
      s"""
      | read_verilog "${vFile.toAbsolutePath}"
      | proc; opt
      | fsm; opt 
      | memory; opt 
      | techmap; opt
      | dfflegalize -cell $$_SR_PP_ x -cell $$_DFF_P_ x -cell $$_DFFSR_PNN_ x
      | clean
      | write_json "${jsonFile.toAbsolutePath}"
    """.stripMargin
    )

    val procLogger = ProcessLogger(_ => ())
    Process("yosys -p 'script script.ys'", tmpDir.toFile).!(procLogger) match {
      case 0 => // Success
      case _ => throw new RuntimeException(s"Yosys failed to process $vFile")
    }
    fromYosysJsonFile(jsonFile)
  }

  def fromYosysJsonFile(jsonFile: Path): ComponentCreator =
    ComponentCreator(Design.fromJsonFile(jsonFile))
}
