package simulator

/** Gate-level simulation delays, in ticks (1 tick = 1 nanosecond).
  *
  * @param wireDelay
  *   ticks for a driven value to propagate through a wire group
  * @param gateDelay
  *   ticks for a gate to react to an input change
  * @param scTolerance
  *   ticks to wait before reporting a short-circuit on a multiply-driven wire group
  */
case class Config(wireDelay: Int, gateDelay: Int, scTolerance: Int)

object Config {
  val default = Config(1, 1, 50)
}
