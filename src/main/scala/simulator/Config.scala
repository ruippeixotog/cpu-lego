package simulator

case class Config(wireDelay: Int, gateDelay: Int, scTolerance: Int)

object Config {
  val default = Config(1, 1, 50)
}
