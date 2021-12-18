package core

class Port

sealed trait LogicLevel extends Port
case object High extends LogicLevel
case object Low extends LogicLevel

sealed trait Component

class NAND extends Component {
  val in1, in2, out = new Port
}

class Clock(val freq: Int) extends Component {
  val out = new Port
}

trait CompositeComponent extends Component {
  def components: List[Component]
  def wires: List[(Port, Port)]
}
