package simulator

import core.*

/** The reference live simulator: a thread-safe [[Sim]] running full gate-level simulation, paced against the wall
  * clock.
  *
  * A thin subclass of [[LiveSim]] over the gate-level [[Engine]]: all of the live, concurrent machinery — the inbox,
  * the paced loop, the published state, the notifier — lives in `LiveSim`; `RefSim` only chooses the gate-level engine.
  *
  * @param circuit
  *   the gate graph to simulate
  * @param conf
  *   gate and wire delays
  * @param ticksPerSecond
  *   simulation ticks per wall-clock second while running
  */
final class RefSim(
    circuit: Circuit,
    conf: Config = Config.default,
    ticksPerSecond: Long = 1000
) extends LiveSim(() => GateEngine(GateProcessor.setup(circuit, conf)), ticksPerSecond)

object RefSim {

  /** Build a reference live simulator for a circuit.
    *
    * @param circuit
    *   the gate graph to simulate
    * @param conf
    *   gate and wire delays
    * @param ticksPerSecond
    *   simulation ticks per wall-clock second while running
    */
  def apply(
      circuit: Circuit,
      conf: Config = Config.default,
      ticksPerSecond: Long = 1000
  ): RefSim =
    new RefSim(circuit, conf, ticksPerSecond)
}
