package util

import java.io.PrintStream
import java.nio.file.{Path, Paths}

import scala.collection.mutable

import component.*
import component.BuilderAPI.*
import core.*
import util.Implicits.toInt
import yosys.ComponentCreator

object GraphvizDrawer {

  class PortGraphIndex(rootComp: CompositeComponent, depth: Int) {
    private val uf = allWires(rootComp, depth).foldLeft(UnionFind[Port]())(_.merge.tupled(_))
    private val portNodes = mutable.Map[Port, mutable.Set[(Direction, String)]]()

    def add(port: Port, dir: Direction, name: String): Unit = {
      portNodes.getOrElseUpdate(uf.root(port), mutable.Set()) += ((dir, name))
    }

    def portNames: Iterator[(Port, Seq[(Direction, String)])] =
      portNodes.view.mapValues(_.toSeq).iterator

    def portHigh: Port = uf.root(High)
    def portLow: Port = uf.root(High)

    private def allWires(comp: Component, depth: Int): Iterator[(Port, Port)] = comp match {
      case CompositeComponent(_, components, wires, _) if depth >= 0 =>
        wires.iterator ++ components.values.flatMap(c => allWires(c, depth - 1))
      case _ =>
        Iterator.empty
    }
  }

  def toDot(rootComp: CompositeComponent, depth: Int = 0): String = {
    val portMap = new PortGraphIndex(rootComp, depth)

    val builder = new StringBuilder()
      .append(s"digraph \"")
      .append(rootComp.name)
      .append("\" {\n")

    // Add nodes for named ports
    rootComp.namedPorts.foreach { case (name, portOrBus) =>
      val cName = s"rc$name"
      portOrBus match {
        case (Some(dir), port: Port) =>
          portMap.add(port, reverse(dir), cName)
          addPortNode(cName, name)
        case (Some(dir), bus: Bus) =>
          builder.append("// Unsupported bus\n")
        case (None, portOrBus) =>
          builder.append(s"// No dir for ${portOrBus}\n")
      }
    }

    def addPortNode(name: String, label: String): Unit = {
      builder.append(s"""$name [ shape=octagon, label="$label", color="black", fontcolor="black"];\n""")
    }

    def addRecordNode(name: String, lPorts: String, label: String, rPorts: String): Unit = {
      builder.append(
        s"""$name [ shape=record, label="{{$lPorts}|$label|{$rPorts}}", color="black", fontcolor="black"];\n"""
      )
    }

    def aux(label: String, comp: CompositeComponent, idPrefix: String, depth: Int): Unit = {
      builder.append("rankdir=\"LR\";\n")
      builder.append("remincross=true;\n")
      builder.append(s"label=\"${label}\";\n")

      // Add nodes for components
      comp.components.foreach { case (name, comp) =>
        val cName = s"${idPrefix}c${name.replaceAll("\\$", "")}"
        comp match {
          case NAND(in1, in2, out) =>
            portMap.add(in1, Direction.Input, s"$cName:p1")
            portMap.add(in2, Direction.Input, s"$cName:p2")
            portMap.add(out, Direction.Output, s"$cName:p3")
            addRecordNode(cName, "<p1> in1|<p2> in2", s"$name\\nNAND", "<p3> out")

          case FlipFlop(set, reset, q, nq) =>
            portMap.add(set, Direction.Input, s"$cName:p1")
            portMap.add(reset, Direction.Input, s"$cName:p2")
            portMap.add(q, Direction.Output, s"$cName:p3")
            portMap.add(nq, Direction.Output, s"$cName:p4")
            addRecordNode(cName, "<p1> set|<p2> reset", s"$name\\nFlipFlop", "<p3> q|<p4> nq")

          case Clock(_, out) =>
            portMap.add(out, Direction.Input, s"$cName:p1")
            addRecordNode(cName, "", s"$name\nClock", "<p1> out")

          case Switch(in, out, enable) =>
            portMap.add(in, Direction.Input, s"$cName:p1")
            portMap.add(enable, Direction.Input, s"$cName:p2")
            portMap.add(out, Direction.Output, s"$cName:p3")
            addRecordNode(cName, "<p1> in|<p2> enable", s"$name\\nSwitch", "<p3> out")

          case cc: CompositeComponent if depth > 0 =>
            builder
              .append("subgraph \"")
              .append(s"cluster_$cName")
              .append("\" {\n")
              .append("style=dashed;")
            aux(name, cc, s"$idPrefix${cName}__", depth - 1)
            builder.append("}\n")

          case cc: CompositeComponent =>
            val ps = cc.namedPorts
              .flatMap {
                case (name, (dir, port: Port)) => List((name, (dir, port)))
                case (name, (dir, bus: Bus)) => bus.zipWithIndex.map { (port, i) => (s"$name[$i]", (dir, port)) }
              }
              .zipWithIndex
              .flatMap {
                case ((name, (Some(dir), port)), i) =>
                  portMap.add(port, dir, s"$cName:p${i + 1}")
                  List((dir, s"""<p${i + 1}> $name"""))
                case _ =>
                  builder.append(s"// No dir for port ${name} in component ${cName}\n")
                  Nil
              }
            val psByDir = ps.groupBy(_._1).mapValues(_.map(_._2))
            val inPs =
              (psByDir.getOrElse(Direction.Input, Nil) ++ psByDir.getOrElse(Direction.Inout, Nil))
                .mkString("|")
            val outPs = psByDir.getOrElse(Direction.Output, Nil).mkString("|")
            addRecordNode(cName, inPs, name, outPs)
        }
      }
    }

    aux(rootComp.name, rootComp, "", depth)

    // Add edges for wires
    def link(dir1: Direction, name1: String, dir2: Direction, name2: String): Unit = {
      val p1 = name1 + (if (dir1 == Direction.Output) ":e" else ":w")
      val p2 = name2 + (if (dir2 == Direction.Output) ":e" else ":w")
      val link =
        if (dir1 == Direction.Output || dir2 == Direction.Input) s"$p1 -> $p2"
        else s"$p2 -> $p1"

      builder.append(s"""$link [color="black", fontcolor="black", label=""];\n""")
    }

    def linkConst(name1: String, ll: LogicLevel): Unit = {
      val kName = s"k${name1.replaceAll(":", "_")}"
      builder.append(s"$kName [ label=\"1'${ll.toInt}\" ];")
      builder.append(s"""$kName -> $name1 [color="black", fontcolor="black", label=""];\n""")
    }

    portMap.portNames.foreach { case (port, names) =>
      if (port == portMap.portHigh) {
        names.foreach { case (_, name) => linkConst(name, High) }
      } else if (port == portMap.portLow) {
        names.foreach { case (_, name) => linkConst(name, Low) }
      } else if (names.size == 1) {
        builder.append(s"// Port not connected: ${names.head._2}\n")
      } else if (
        names.size == 2 || names.count(_._1 == Direction.Input) == 1 || names.count(_._1 == Direction.Output) == 1
      ) {
        for {
          (dir1, name1) <- names.filter(_._1 == Direction.Input)
          (dir2, name2) <- names.filter(_._1 != Direction.Input)
        } link(dir1, name1, dir2, name2)
      } else {
        val ptName = s"n${port.hashCode()}";
        builder.append(s"$ptName [ shape=point ];\n")
        names.foreach { case (dir1, name1) =>
          link(dir1, name1, reverse(dir1), ptName)
        }
      }
    }

    builder.append("}\n")
    builder.toString()
  }

  private def reverse(dir: Direction): Direction = dir match {
    case Direction.Input => Direction.Output
    case Direction.Output => Direction.Input
    case Direction.Inout => Direction.Inout
  }
}
