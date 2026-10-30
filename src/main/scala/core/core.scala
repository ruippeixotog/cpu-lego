package core

class Port

sealed trait LogicLevel extends Port
case object High extends LogicLevel
case object Low extends LogicLevel

type Bus = Vector[Port]

sealed trait Component
sealed trait BaseComponent extends Component

case class NAND(in1: Port, in2: Port, out: Port) extends BaseComponent

/** A clock signal: a square wave that toggles every `halfPeriod` ticks.
  *
  * @param halfPeriod
  *   ticks between consecutive toggles; the full period is twice this
  * @param out
  *   the clock output port
  * @param initialLevel
  *   the level driven at t=0; defaults to High, the historical behaviour
  */
case class Clock(halfPeriod: Int, out: Port, initialLevel: Boolean = true) extends BaseComponent

case class Switch(in: Port, out: Port, enable: Port) extends BaseComponent

enum Direction {
  case Input, Output, Inout
}

case class CompositeComponent(
    name: String,
    components: Map[String, Component],
    wires: List[(Port, Port)],
    namedPorts: Map[String, (Option[Direction], Port | Bus)]
) extends Component
