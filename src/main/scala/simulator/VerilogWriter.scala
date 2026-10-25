package simulator

import scala.collection.mutable

import core.*

/** Structural Verilog export of a flat [[Netlist]], so DSL designs can be checked with external tools: simulated with
  * Verilator or Icarus, or proved equivalent to a behavioural model with yosys on combinational blocks.
  *
  * The export is a single module whose ports are the netlist's top-level pins:
  *   - a NAND becomes `assign y = ~(a & b);`
  *   - a Switch becomes `assign y = en ? a : 1'bz;` (a net driven by several switches, or by a switch and other gates,
  *     resolves exactly as a Verilog wire does)
  *   - a Clock is not exported — it has no structural equivalent: its output net is declared but left undriven, and a
  *     comment records its half period, so the net can be driven from a testbench instead
  *   - the constant nets are read as the literals `1'b0` and `1'b1`
  *
  * Pins registered as one bus (`name[0]` .. `name[n-1]`, all with the same direction and on distinct nets) collapse
  * back into a single vector port; every other pin is a scalar port. A pin without a registered direction is an output
  * if its net is driven inside the design and an input otherwise; a pin tied to a constant net is an output tied to
  * that constant.
  *
  * Internal nets keep their hierarchical names as wire names. A name that is not a legal Verilog identifier (the
  * hierarchical names contain `.` and `[`) or that collides with a Verilog keyword is written as an escaped identifier,
  * so names survive the round trip through a Verilog parser unchanged.
  *
  * If several pins land on one net, the first pin is the one read and driven in the export; the others are tied to it
  * with an `assign` when they are outputs or inouts. Two input pins on one net cannot be aliased inside a Verilog
  * module — that case is flagged with a comment and only the first pin is connected.
  */
object VerilogWriter {

  /** The Verilog source of `netlist`, as a module named `moduleName`. */
  def write(netlist: Netlist, moduleName: String): String = {
    val pins = netlist.pins
    val dirs = pins.map(pin => directionOf(netlist, pin))
    val vectorBitOf = vectorBits(netlist, pins, dirs)
    val pinRefs = pins.indices.map { i =>
      vectorBitOf.get(i) match {
        case Some((base, bit)) => s"${escapeName(base)}[$bit]"
        case None => escapeName(pins(i).name)
      }
    }
    val decls = declarations(pins, dirs, vectorBitOf)

    // The first pin on each net is the one read and driven in the export.
    val primaryPinOf = mutable.Map[Int, Int]()
    pins.indices.foreach(i => primaryPinOf.getOrElseUpdate(pins(i).net, i))

    val usedNames = mutable.Set[String]()
    pins.indices.foreach(i => usedNames += vectorBitOf.get(i).map(_._1).getOrElse(pins(i).name))

    val refs = new Array[String](netlist.netCount)
    refs(netlist.lowNet) = "1'b0"
    refs(netlist.highNet) = "1'b1"
    for (net <- 2 until netlist.netCount) {
      primaryPinOf.get(net) match {
        case Some(i) => refs(net) = pinRefs(i)
        case None =>
          var candidate = netlist.nameOf(net).getOrElse(s"n$net")
          if (usedNames.contains(candidate)) candidate = s"n$net"
          while (usedNames.contains(candidate)) candidate += "_"
          usedNames += candidate
          refs(net) = escapeName(candidate)
      }
    }

    val body = Vector.newBuilder[String]
    for (net <- 2 until netlist.netCount if !primaryPinOf.contains(net))
      body += s"wire ${refs(net)};"

    for (i <- netlist.nandOut.indices) {
      val out = netlist.nandOut(i)
      if (isConstantNet(netlist, out))
        body += s"// omitted NAND: its output is shorted to the constant ${refs(out)} net"
      else
        body += s"assign ${refs(out)} = ~(${refs(netlist.nandIn1(i))} & ${refs(netlist.nandIn2(i))});"
    }

    for (i <- netlist.switchOut.indices) {
      val out = netlist.switchOut(i)
      if (isConstantNet(netlist, out))
        body += s"// omitted switch: its output is shorted to the constant ${refs(out)} net"
      else
        body += s"assign ${refs(out)} = ${refs(netlist.switchEn(i))} ? ${refs(netlist.switchIn(i))} : 1'bz;"
    }

    // Constant pins are tied to their rail; secondary pins are tied to the primary pin of their net.
    pins.indices.foreach { i =>
      val pin = pins(i)
      if (isConstantNet(netlist, pin.net))
        body += s"assign ${pinRefs(i)} = ${refs(pin.net)};"
      else if (primaryPinOf(pin.net) != i) {
        dirs(i) match {
          case Direction.Input =>
            body += s"// ${pin.name} and ${pins(primaryPinOf(pin.net)).name} are one net in the source netlist; " +
              "two input ports cannot be aliased inside a module, so only the latter is connected"
          case _ =>
            body += s"assign ${pinRefs(i)} = ${pinRefs(primaryPinOf(pin.net))};"
        }
      }
    }

    for (i <- netlist.clockOut.indices)
      body += s"// clock with half period ${netlist.clockHalfPeriod(i)} ticks driving ${refs(netlist.clockOut(i))} " +
        "is not exported; drive the net from a testbench"

    val header =
      if (decls.isEmpty) s"module ${escapeName(moduleName)}();"
      else decls.mkString(s"module ${escapeName(moduleName)}(\n    ", ",\n    ", "\n);")
    (header +: body.result().map("  " + _) :+ "endmodule").mkString("\n") + "\n"
  }

  private def isConstantNet(netlist: Netlist, net: Int): Boolean =
    net == netlist.lowNet || net == netlist.highNet

  private def directionOf(netlist: Netlist, pin: Netlist.Pin): Direction =
    if (isConstantNet(netlist, pin.net)) Direction.Output
    else
      pin.direction.getOrElse {
        if (netlist.driversOf(pin.net).nonEmpty) Direction.Output else Direction.Input
      }

  private val IndexedName = """(.*)\[(\d+)\]""".r

  private def indexedName(name: String): Option[(String, Int)] =
    name match {
      case IndexedName(base, bit) => Some((base, bit.toInt))
      case _ => None
    }

  /** Pin index -> (bus base name, bit index), for the pins that collapse into a vector port: a group of pins named
    * `base[0]` .. `base[n-1]` (n >= 2) with one shared direction, on distinct non-constant nets, where no other pin
    * takes the base name itself.
    */
  private def vectorBits(
      netlist: Netlist,
      pins: Vector[Netlist.Pin],
      dirs: Vector[Direction]
  ): Map[Int, (String, Int)] = {
    val byBase = mutable.Map[String, Vector[(Int, Int)]]()
    pins.indices.foreach { i =>
      indexedName(pins(i).name).foreach { case (base, bit) =>
        byBase(base) = byBase.getOrElse(base, Vector.empty) :+ (i, bit)
      }
    }
    val result = mutable.Map[Int, (String, Int)]()
    byBase.foreach { case (base, members) =>
      val memberPins = members.map(_._1).toSet
      val ok = members.size >= 2 &&
        members.map(_._2).sorted == (0 until members.size).toVector &&
        members.map(i => pins(i._1).net).distinct.size == members.size &&
        members.forall(i => !isConstantNet(netlist, pins(i._1).net)) &&
        members.map(i => dirs(i._1)).distinct.size == 1 &&
        pins.indices.forall(j => memberPins.contains(j) || pins(j).name != base)
      if (ok) members.foreach { case (i, bit) => result(i) = (base, bit) }
    }
    result.toMap
  }

  private def declarations(
      pins: Vector[Netlist.Pin],
      dirs: Vector[Direction],
      vectorBitOf: Map[Int, (String, Int)]
  ): Vector[String] = {
    val declaredBases = mutable.Set[String]()
    pins.indices.flatMap { i =>
      vectorBitOf.get(i) match {
        case Some((base, _)) =>
          if (!declaredBases.add(base)) None
          else {
            val width = vectorBitOf.count(_._2._1 == base)
            Some(s"${directionKeyword(dirs(i))} [${width - 1}:0] ${escapeName(base)}")
          }
        case None =>
          Some(s"${directionKeyword(dirs(i))} ${escapeName(pins(i).name)}")
      }
    }.toVector
  }

  private def directionKeyword(dir: Direction): String =
    dir match {
      case Direction.Input => "input"
      case Direction.Output => "output"
      case Direction.Inout => "inout"
    }

  private val SimpleName = "[A-Za-z_][A-Za-z0-9_$]*".r

  /** The Verilog identifier for `name`: the name itself when it is a legal simple identifier and not a keyword,
    * otherwise an escaped identifier (a backslash, the name, and a terminating space), which Verilog parsers accept for
    * arbitrary names.
    */
  private def escapeName(name: String): String =
    name match {
      case SimpleName() if !Keywords.contains(name) => name
      case _ => s"\\$name "
    }

  // Verilog-2001 keywords.
  private val Keywords = Set(
    "always",
    "and",
    "assign",
    "automatic",
    "begin",
    "buf",
    "bufif0",
    "bufif1",
    "case",
    "casex",
    "casez",
    "cell",
    "cmos",
    "config",
    "deassign",
    "default",
    "defparam",
    "design",
    "disable",
    "edge",
    "else",
    "end",
    "endcase",
    "endconfig",
    "endfunction",
    "endgenerate",
    "endmodule",
    "endprimitive",
    "endspecify",
    "endtable",
    "endtask",
    "event",
    "for",
    "force",
    "forever",
    "fork",
    "function",
    "generate",
    "genvar",
    "highz0",
    "highz1",
    "if",
    "ifnone",
    "incdir",
    "include",
    "initial",
    "inout",
    "input",
    "instance",
    "integer",
    "join",
    "large",
    "liblist",
    "library",
    "localparam",
    "macromodule",
    "medium",
    "module",
    "nand",
    "negedge",
    "nmos",
    "nor",
    "noshowcancelled",
    "not",
    "notif0",
    "notif1",
    "or",
    "output",
    "parameter",
    "pmos",
    "posedge",
    "primitive",
    "pull0",
    "pull1",
    "pulldown",
    "pullup",
    "pulsestyle_ondetect",
    "pulsestyle_onevent",
    "rcmos",
    "real",
    "realtime",
    "reg",
    "release",
    "repeat",
    "rnmos",
    "rpmos",
    "rtran",
    "rtranif0",
    "rtranif1",
    "scalared",
    "showcancelled",
    "signed",
    "small",
    "specify",
    "specparam",
    "strong0",
    "strong1",
    "supply0",
    "supply1",
    "table",
    "task",
    "time",
    "tran",
    "tranif0",
    "tranif1",
    "tri",
    "tri0",
    "tri1",
    "triand",
    "trior",
    "trireg",
    "units",
    "unsigned",
    "use",
    "vectored",
    "wait",
    "wand",
    "weak0",
    "weak1",
    "while",
    "wire",
    "wor",
    "xnor",
    "xor"
  )
}
