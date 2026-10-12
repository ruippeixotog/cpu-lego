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
  * @param onFault
  *   what to do when the engine reports a fault: [[FaultPolicy.Halt]] (the default) stops the simulation loop on the
  *   first fault, [[FaultPolicy.Continue]] keeps simulating and only notifies [[Sim.faults]] subscribers
  * @param portNames
  *   hierarchical port names used in fault reports; ports missing from the map fall back to an identity-based
  *   placeholder. Build one from a root component with `Index(root).ports.map(_.swap).toMap`.
  */
final class RefSim(
    circuit: Circuit,
    conf: Config = Config.default,
    ticksPerSecond: Long = 1000,
    onFault: FaultPolicy = FaultPolicy.Halt,
    portNames: Map[Port, String] = Map.empty
) extends LiveSim(() => GateEngine(GateProcessor.setup(circuit, conf, portNames)), ticksPerSecond, onFault)

object RefSim {

  /** Build a reference live simulator for a circuit.
    *
    * @param circuit
    *   the gate graph to simulate
    * @param conf
    *   gate and wire delays
    * @param ticksPerSecond
    *   simulation ticks per wall-clock second while running
    * @param onFault
    *   what to do when the engine reports a fault: [[FaultPolicy.Halt]] (the default) stops the simulation loop on the
    *   first fault, [[FaultPolicy.Continue]] keeps simulating and only notifies [[Sim.faults]] subscribers
    * @param portNames
    *   hierarchical port names used in fault reports; ports missing from the map fall back to an identity-based
    *   placeholder. Build one from a root component with `Index(root).ports.map(_.swap).toMap`.
    */
  def apply(
      circuit: Circuit,
      conf: Config = Config.default,
      ticksPerSecond: Long = 1000,
      onFault: FaultPolicy = FaultPolicy.Halt,
      portNames: Map[Port, String] = Map.empty
  ): RefSim =
    new RefSim(circuit, conf, ticksPerSecond, onFault, portNames)
}
