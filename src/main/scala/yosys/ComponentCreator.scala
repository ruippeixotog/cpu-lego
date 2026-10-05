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

  private def createFromType(
      compName: String,
      cell: Design.Cell,
      inPorts: Map[String, Port | Bus]
  ): Spec[Map[String, Port | Bus]] = {
    // https://yosyshq.readthedocs.io/projects/yosys/en/0.32/CHAPTER_CellLib.html
    // Semantics below transcribed from techlibs/common/simcells.v.
    def inPort(name: String): Port = inPorts(name).asInstanceOf[Port]
    def inPortsOf(names: String): Vector[Port] = names.map(c => inPort(c.toString)).toVector
    cell.`type` match {
      case "$_AND_" =>
        Map("Y" -> and(inPort("A"), inPort("B")))
      case "$_ANDNOT_" =>
        Map("Y" -> andNot(inPort("A"), inPort("B")))
      case "$_AOI3_" =>
        Map("Y" -> not(or(and(inPort("A"), inPort("B")), inPort("C"))))
      case "$_AOI4_" =>
        Map(
          "Y" -> not(
            or(and(inPort("A"), inPort("B")), and(inPort("C"), inPort("D")))
          )
        )
      case "$_BUF_" =>
        Map("Y" -> inPort("A"))
      case "$_MUX_" =>
        Map("Y" -> mux(Vector(inPort("A"), inPort("B")), Vector(inPort("S"))))
      case "$_MUX4_" =>
        Map("Y" -> mux(inPortsOf("ABCD"), inPortsOf("ST")))
      case "$_MUX8_" =>
        Map("Y" -> mux(inPortsOf("ABCDEFGH"), inPortsOf("STU")))
      case "$_MUX16_" =>
        Map("Y" -> mux(inPortsOf("ABCDEFGHIJKLMNOP"), inPortsOf("STUV")))
      case "$_NAND_" =>
        Map("Y" -> nand(inPort("A"), inPort("B")))
      case "$_NMUX_" =>
        Map("Y" -> not(mux(Vector(inPort("A"), inPort("B")), Vector(inPort("S")))))
      case "$_NOR_" =>
        Map("Y" -> nor(inPort("A"), inPort("B")))
      case "$_NOT_" =>
        Map("Y" -> not(inPort("A")))
      case "$_OAI3_" =>
        Map("Y" -> not(and(or(inPort("A"), inPort("B")), inPort("C"))))
      case "$_OAI4_" =>
        Map(
          "Y" -> not(
            and(or(inPort("A"), inPort("B")), or(inPort("C"), inPort("D")))
          )
        )
      case "$_OR_" =>
        Map("Y" -> or(inPort("A"), inPort("B")))
      case "$_ORNOT_" =>
        Map("Y" -> orNot(inPort("A"), inPort("B")))
      case "$_SR_PP_" =>
        Map("Q" -> norLatch(inPort("S"), inPort("R"))._1)
      case "$_DFF_P_" =>
        Map("Q" -> dLatch(inPort("D"), inPort("C"))._1)
      case "$_DFFSR_PNN_" =>
        Map(
          "Q" -> dLatch(
            inPort("D"),
            inPort("C"),
            inPort("R"),
            inPort("S")
          )._1
        )
      case "$_XNOR_" =>
        Map("Y" -> xnor(inPort("A"), inPort("B")))
      case "$_XOR_" =>
        Map("Y" -> xor(inPort("A"), inPort("B")))
      case m if !m.startsWith("$") =>
        create(m, inPorts)
      case _ =>
        val src = cell.attributes.get("src").map(s => s", src $s").getOrElse("")
        throw new IllegalArgumentException(s"Unsupported component type: ${cell.`type`} (cell $compName$src)")
    }
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
