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

class Clock(val freq: Int) extends BaseComponent {
  val out = new Port
}

class PosEdge extends BaseComponent {
  val in, out = new Port
}

trait CompositeComponent extends Component {
  def components: List[Component]
  def wires: List[(Port, Port)]
}
