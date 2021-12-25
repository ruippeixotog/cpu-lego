package core

class Port

sealed trait LogicLevel extends Port
case object High extends LogicLevel
case object Low extends LogicLevel

sealed trait Component
sealed trait BaseComponent extends Component

class NAND extends BaseComponent {
  val in1, in2, out = new Port
}

class Flipflop extends BaseComponent { self =>
  val set, reset, q, nq = new Port
}

class Clock(val freq: Int) extends BaseComponent {
  val out = new Port
}

class PosEdge extends BaseComponent {
  val in, out = new Port
}

class Switch extends BaseComponent {
  val in, out, enable = new Port
}

trait CompositeComponent extends Component {
  def components: List[Component]
  def wires: List[(Port, Port)]
}
