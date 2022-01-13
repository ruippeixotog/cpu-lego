package core

class Port

sealed trait LogicLevel extends Port
case object High extends LogicLevel
case object Low extends LogicLevel

type Bus = Vector[Port]

sealed trait Component
sealed trait BaseComponent extends Component

case class NAND(in1: Port, in2: Port, out: Port) extends BaseComponent
case class FlipFlop(set: Port, reset: Port, q: Port, nq: Port) extends BaseComponent
case class Clock(freq: Int, out: Port) extends BaseComponent
case class PosEdge(in: Port, out: Port) extends BaseComponent
case class Switch(in: Port, out: Port, enable: Port) extends BaseComponent

case class CompositeComponent(
    name: String,
    components: Map[String, Component],
    wires: List[(Port, Port)],
    namedPorts: Map[String, Port | Bus]
) extends Component
