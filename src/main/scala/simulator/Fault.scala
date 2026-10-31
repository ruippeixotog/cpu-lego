package simulator

/** What to do when a fault is reported. */
enum FaultPolicy {

  /** Stop the simulation loop on the first fault: [[Sim.isRunning]] becomes false and [[Sim.failure]] completes with
    * it.
    */
  case Halt

  /** Keep simulating: faults are only delivered to [[Sim.faults]] subscribers and never stop the loop. */
  case Continue
}

/** A structured fault: a condition in a simulated design or its execution, reported as data instead of through
  * ad-hoc failure paths (printed warnings, thrown exceptions), so a fault never kills a simulation silently.
  * Examples of use: a compiled engine reporting a net it could not settle, or a consumer halting on the first fault.
  */
sealed trait Fault

object Fault {

  /** Two or more drivers drive the same net with conflicting values.
    *
    * @param nets
    *   hierarchical names of the contended net's ports
    * @param values
    *   the conflicting driven values, sorted
    */
  final case class Contention(nets: List[String], values: List[Boolean]) extends Fault

  /** A net did not settle within a convergence bound — for example, a compiled engine failing to settle a net. */
  final case class NonConvergence(nets: List[String]) extends Fault

  /** A device callback threw.
    *
    * TODO(#16, #17): wire this up once devices exist. Peripheral watch callbacks keep the established print-and-ignore
    * convention until then.
    *
    * @param device
    *   the name of the device whose callback failed
    * @param throwable
    *   what the callback threw
    */
  final case class DeviceFailure(device: String, throwable: Throwable) extends Fault
}
