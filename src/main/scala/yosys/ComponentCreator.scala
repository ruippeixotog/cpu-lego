package yosys

import java.nio.file.{Files, Path}

import scala.collection.mutable
import scala.sys.process.*

import component.*
import component.BuilderAPI.*
import component.BuilderAPIMacros.NamedPort
import core.*
import yosys.Design.Direction as DesignDirection

case class ComponentCreator(design: Design) {

  def create(moduleName: String, inPorts: Map[String, Port | Bus]): Spec[Map[String, Port | Bus]] = {
    val env = summon[BuilderEnv]
    val module = design.modules(moduleName)
    val portReg = mutable.Map[Int | String, Port]("0" -> Low, "1" -> High)

    // Visible netnames by bit, for debuggable names in the built component. The display name prefers
    // the `hdlname` attribute when present; hidden names (hide_name: 1) are skipped. Sorted by net
    // name for determinism; the first display name wins when several nets alias the same bits.
    val netNamesByBit: Map[Int, String] = {
      val byBit = mutable.Map[Int, String]()
      val seenNames = mutable.Set[String]()
      module.netnames.toList.sortBy(_._1).foreach { case (name, net) =>
        if (net.hideName != 1 && net.bits.forall(_.isInstanceOf[Int])) {
          val display = net.attributes.get("hdlname").getOrElse(name)
          if (seenNames.add(display))
            net.bits.foreach {
              case bit: Int => byBit.getOrElseUpdate(bit, display)
              case _ => // unreachable: filtered above
            }
        }
      }
      byBit.toMap
    }

    // Wire port for a net bit, named from the netnames when the bit has a visible name.
    def netPort(bit: Int | String): Port =
      portReg.getOrElseUpdate(
        bit,
        bit match {
          case b: Int => netNamesByBit.get(b).fold(new Port())(n => NamedPort(Some(moduleName), n))
          case _ => new Port()
        }
      )

    // Register visible nets as named ports (no direction: they are internal, not boundary), so the
    // Index and future fault/lint reports show RTL-meaningful names. Boundary ports registered below
    // take precedence on name collisions.
    netNamesByBit.toList
      .groupBy(_._2)
      .toList
      .sortBy(_._1)
      .foreach { case (name, bits) =>
        val ports = bits.map(_._1).sorted.map(netPort)
        if (ports.length == 1) env.register(name, ports.head, None)
        else env.register(name, ports.toVector, None)
      }

    inPorts.foreach { case (name, p) =>
      val dir = module.ports(name).direction match {
        case DesignDirection.Input => Direction.Input
        case DesignDirection.Output => Direction.Output
        case DesignDirection.Inout => Direction.Inout
      }
      env.register(name, p, Some(dir))
    }

    inPorts.foreach {
      case (name, p: Port) =>
        // println(s"[in] Connecting $name to ${module.ports(name).bits.head}")
        p ~> netPort(module.ports(name).bits.head)
      case (name, p: Bus) =>
        module.ports(name).bits.zipWithIndex.foreach { (bit, i) => p(i) ~> netPort(bit) }
    }

    val components = module.cells.map { case (compName, cell) =>
      val portDirs =
        if (cell.portDirections.nonEmpty) cell.portDirections
        else ComponentCreator.knownPortDirections(cell.`type`)

      val cellInPorts = portDirs
        .filter(_._2 != DesignDirection.Output)
        .map { case (name, _) =>
          name -> (cell.connections(name) match {
            case Vector(bit) => netPort(bit)
            case bits => bits.map(netPort)
          })
        }

      val outPorts = createFromType(compName, cell, cellInPorts)

      outPorts.foreach {
        case (name, p: Port) =>
          // println(s"[comp $compName] Connecting $name to ${cell.connections(name).head}")
          p ~> netPort(cell.connections(name).head)
        case (name, p: Bus) =>
          cell.connections(name).zipWithIndex.foreach { (bit, i) => p(i) ~> netPort(bit) }
      }
    }

    val outPorts = module.ports.toList.flatMap { case (name, port) =>
      port.direction match {
        case DesignDirection.Input =>
          None
        case DesignDirection.Output | DesignDirection.Inout =>
          val dir = if (port.direction == DesignDirection.Inout) Direction.Inout else Direction.Output
          val p = port.bits match {
            case Vector(bit) =>
              if (port.direction == DesignDirection.Inout) netPort(bit) else portReg(bit)
            case _ =>
              if (port.direction == DesignDirection.Inout) port.bits.map(netPort) else port.bits.map(portReg)
          }
          Some((name, dir, p))
      }
    }
    outPorts.foreach { case (name, dir, p) => env.register(name, p, Some(dir)) }
    outPorts.map { case (name, _, p) => name -> p }.toMap
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
    // Cell types instantiated directly in Verilog keep Yosys's backslash escape (e.g. `\$_TBUF_`).
    cell.`type`.stripPrefix("\\") match {
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
      case "$_TBUF_" | "$tribuf" =>
        // Tri-state buffer: several TBUFs driving one net form a normal shared bus, which the
        // wire group resolves (at most one driver enabled at a time).
        Map("Y" -> switch(inPort("A"), inPort("E")))
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
        // Submodule instance: keep it as a named subcomponent (unless its name is hidden) so the
        // Index and future fault/lint reports show RTL-meaningful paths.
        if (cell.hideName == 1) create(m, inPorts)
        else {
          val outerEnv = summon[BuilderEnv]
          val innerSpec: Spec[Map[String, Port | Bus]] = create(m, inPorts)
          val (subOut, subComp) = buildComponent(Some(compName), innerSpec)
          outerEnv.add(compName, subComp)
          subOut
        }
      case _ =>
        val src = cell.attributes.get("src").map(s => s", src $s").getOrElse("")
        throw new IllegalArgumentException(s"Unsupported component type: ${cell.`type`} (cell $compName$src)")
    }
  }
}

object ComponentCreator {

  /** Port directions for cell types whose JSON omits `port_directions` (e.g. `$_TBUF_` instantiated directly in Verilog
    * rather than produced by techmap). Unknown types map to empty, and the caller fails clearly when it looks up a
    * missing port.
    */
  private def knownPortDirections(cellType: String): Map[String, Design.Direction] =
    cellType.stripPrefix("\\") match {
      case "$_TBUF_" | "$tribuf" =>
        Map("A" -> Design.Direction.Input, "E" -> Design.Direction.Input, "Y" -> Design.Direction.Output)
      case _ => Map.empty
    }

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
