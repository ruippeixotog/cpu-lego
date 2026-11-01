package core

class Port

sealed trait LogicLevel extends Port
case object High extends LogicLevel
case object Low extends LogicLevel

/** Raw, unsized ports: what the netlist, the simulators and importers work on. */
type Ports = Vector[Port]

sealed trait Component
sealed trait BaseComponent extends Component

case class NAND(in1: Port, in2: Port, out: Port) extends BaseComponent
case class Clock(freq: Int, out: Port) extends BaseComponent
case class Switch(in: Port, out: Port, enable: Port) extends BaseComponent

enum Direction {
  case Input, Output, Inout
}

case class CompositeComponent(
    name: String,
    components: Map[String, Component],
    wires: List[(Port, Port)],
    namedPorts: Map[String, (Option[Direction], Port | Ports)]
) extends Component
