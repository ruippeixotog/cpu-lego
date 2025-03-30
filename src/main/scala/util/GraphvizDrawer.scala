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

  def test(): Unit = {
    val latchCreator = ComponentCreator(Paths.get("src/test/resources/dlatch.json"))

    def dLatch(d: Port, en: Port, rstn: Port): Spec[Port] = {
      val outs = latchCreator.create("d_latch", Map("d" -> d, "en" -> en, "rstn" -> rstn))
      outs("q").asInstanceOf[Port]
    }

    val d, en, rstn = new Port()
    val (f, comp) = buildComponent { dLatch(d, en, rstn) }

    // val (f, comp) = buildComponent {
    //   val a, b, c, d = newPort()
    //   val and1 = and(a, b)
    //   val and2 = and(c, d)
    //   val or1 = or(and1, and2)
    //   not(or1)
    // }

    val out = new PrintStream("test.dot")
    out.println(toDot(comp))
  }

  def toDot(component: Component): String = {
    val builder = new StringBuilder
    builder
      .append("digraph \"")
      .append(component match {
        case CompositeComponent(name, _, _, _) => name
        case _ => "component"
      })
      .append("\" {\n")
    toDotGraph(builder, component)

    builder.append("}\n")
    builder.toString()
  }

  def toDotGraph(builder: StringBuilder, component: Component): Unit = {
    builder
      .append("label=\"")
      .append(component match {
        case CompositeComponent(name, _, _, _) => name
        case _ => "component"
      })
      .append("\";\n")
    builder.append("rankdir=\"LR\";\n")
    builder.append("remincross=true;\n")

    component match {
      case CompositeComponent(_, components, wires, namedPorts) =>
        val uf = wires.foldLeft(UnionFind[Port]())(_.merge.tupled(_))
        val portNodes = mutable.Map[Port, mutable.Set[(Direction, String)]]()

        // Add nodes for named ports
        namedPorts.foreach { case (name, portOrBus) =>
          val cName = s"rc$name"
          portOrBus match {
            case (Some(dir), port: Port) =>
              portNodes.getOrElseUpdate(uf.root(port), mutable.Set()) += ((reverse(dir), cName))
              builder.append(s"""$cName [ shape=octagon, label="$name", color="black", fontcolor="black"];\n""")
            case (Some(dir), bus: Bus) =>
              builder.append("// Unsupported bus\n")
            // do nothing for unnamed ports
            // bus.foreach { port =>
            //   portNodes(uf.root(port)) += ((dir, cName))
            //   builder.append(s"""$cName [ shape=octagon, label="$name", color="black", fontcolor="black"];\n""")
            // }
            case (None, portOrBus) =>
              builder.append(s"// No dir for ${portOrBus}\n")
            // Do nothing for unnamed ports
          }
        }

        // Add nodes for components
        components.foreach { case (name, comp) =>
          val cName = s"c${name.replaceAll("\\$", "")}"
          val (shape, label) = comp match {
            case NAND(in1, in2, out) =>
              portNodes.getOrElseUpdate(uf.root(in1), mutable.Set()) += ((Direction.Input, s"$cName:p1"))
              portNodes.getOrElseUpdate(uf.root(in2), mutable.Set()) += ((Direction.Input, s"$cName:p2"))
              portNodes.getOrElseUpdate(uf.root(out), mutable.Set()) += ((Direction.Output, s"$cName:p3"))
              ("record", s"""{{<p1> in1|<p2> in2}|$name\nNAND|{<p3> out}}""")
            case FlipFlop(set, reset, q, nq) =>
              portNodes.getOrElseUpdate(uf.root(set), mutable.Set()) += ((Direction.Input, s"$cName:p1"))
              portNodes.getOrElseUpdate(uf.root(reset), mutable.Set()) += ((Direction.Input, s"$cName:p2"))
              portNodes.getOrElseUpdate(uf.root(q), mutable.Set()) += ((Direction.Output, s"$cName:p3"))
              portNodes.getOrElseUpdate(uf.root(nq), mutable.Set()) += ((Direction.Output, s"$cName:p4"))
              ("record", s"""{{<p1> set|<p2> reset}|$name\nFlipFlop|{<p3> q|<p4> nq}}""")
            case Clock(_, out) =>
              portNodes.getOrElseUpdate(uf.root(out), mutable.Set()) += ((Direction.Input, s"$cName:p1"))
              ("record", s"""{{}|$name|{<p1> out}}""")
            case Switch(in, out, enable) =>
              portNodes.getOrElseUpdate(uf.root(in), mutable.Set()) += ((Direction.Input, s"$cName:p1"))
              portNodes.getOrElseUpdate(uf.root(enable), mutable.Set()) += ((Direction.Input, s"$cName:p2"))
              portNodes.getOrElseUpdate(uf.root(out), mutable.Set()) += ((Direction.Output, s"$cName:p3"))
              ("record", s"""{{<p1> in|<p2> enable}|$name\nSwitch|{<p3> out}}""")
            case cc: CompositeComponent =>
              val ps = cc.namedPorts
                .flatMap {
                  case (name, (dir, port: Port)) => List((name, (dir, port)))
                  case (name, (dir, bus: Bus)) => bus.zipWithIndex.map { (port, i) => (s"$name[$i]", (dir, port)) }
                }
                .zipWithIndex
                .flatMap {
                  case ((name, (Some(dir), port)), i) =>
                    portNodes
                      .getOrElseUpdate(uf.root(port), mutable.Set()) += ((dir, s"$cName:p${i + 1}"))
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
              // ("record", s"""{{${inPs}}|$name\\n${cc.name}|{${outPs}}}""")
              ("record", s"""{{${inPs}}|$name|{${outPs}}}""")
            // builder
            //   .append("subgraph \"")
            //   .append(cName)
            //   .append("\" {\n")
            // toDotGraph(builder, cc)
            // builder.append("}\n")
            // ("record", cName)
          }
          builder.append(s"""$cName [ shape=$shape, label="$label" ];\n""")
        }

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

        portNodes.foreach { case (port, names) =>
          if (port == uf.root(High)) {
            names.foreach { case (_, name) => linkConst(name, High) }
          } else if (port == uf.root(Low)) {
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

      case _ =>
        builder.append("// Unsupported component type\n")
    }
  }

  def reverse(dir: Direction): Direction = dir match {
    case Direction.Input => Direction.Output
    case Direction.Output => Direction.Input
    case Direction.Inout => Direction.Inout
  }
}
