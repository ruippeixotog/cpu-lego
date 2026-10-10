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
  * Simulation time is measured in ticks, with 1 tick = 1 nanosecond, so `halfPeriod` is a duration in both ticks and
  * nanoseconds. See [[Clock.halfPeriodForHz]] to derive the half-period from a target frequency.
  *
  * @param halfPeriod
  *   ticks (nanoseconds) between consecutive toggles; the full period is twice this
  * @param out
  *   the clock output port
  * @param initialLevel
  *   the level driven at t=0; defaults to High, the historical behaviour
  */
case class Clock(halfPeriod: Int, out: Port, initialLevel: Boolean = true) extends BaseComponent

object Clock {

  /** Half-period in ticks (nanoseconds) for a clock running at `hz` hertz: `1e9 / (2 * hz)`, rounded to the nearest
    * tick and clamped to at least 1.
    *
    * Rounding means the achieved frequency, `1e9 / (2 * halfPeriod)` Hz, can differ from `hz` by up to half a tick per
    * half-period — negligible for slow clocks, worth double-checking near the 500 MHz ceiling.
    */
  def halfPeriodForHz(hz: Double): Int = {
    require(hz > 0, "hz must be positive")
    math.max(1, math.round(1e9 / (2 * hz)).toInt)
  }
}

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
