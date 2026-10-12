package simulator

/** What a [[Sim]] does when an engine reports a fault. */
enum FaultPolicy {

  /** Stop the simulation loop on the first fault: [[Sim.isRunning]] becomes false and [[Sim.failure]] completes with
    * it.
    */
  case Halt

  /** Keep simulating: faults are only delivered to [[Sim.faults]] subscribers and never stop the loop. */
  case Continue
}

/** A structured fault reported by a simulation engine.
  *
  * Faults replace the ad-hoc failure paths (printed warnings, thrown exceptions) so that a simulation never dies
  * silently: engines record them, and live simulators surface them through [[Sim.faults]] and [[Sim.failure]].
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

  /** An engine failed to settle a net within its convergence bound. Unused by the reference gate-level engine; meant
    * for compiled engines.
    */
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
