package simulator

import core.*

/** A change in a port's effective value, as reported by a [[SimEngine]].
  *
  * @param port
  *   the port whose value changed
  * @param value
  *   the port's new effective value, as read by `get`
  * @param tick
  *   the tick at which the new value became effective
  */
final case class PortUpdate(port: Port, value: Option[Boolean], tick: Long)

/** A backend capable of executing a [[Circuit]].
  *
  * A SimEngine is the seam between the outside world and any simulation
  * implementation: the reference event-driven [[Sim]], a future backend that
  * compiles the gate graph to native code, an FPGA driver, and so on. All
  * interaction with a running simulation goes through this interface (see
  * [[LiveSim]]); what an implementation does with the gates internally is
  * opaque.
  *
  * ===Observable contract===
  * Every implementation must be observably indistinguishable from the
  * reference [[Sim]] through this interface:
  *
  *  - Time: `tick` is monotonic non-decreasing. `runTo(d)` processes every
  *    event batch with timestamp <= d, in timestamp order, and does not
  *    process batches with timestamp > d. `step()` processes the next
  *    scheduled event batch.
  *  - Drives: `set(port, v)` schedules a drive that is applied at the
  *    current tick, before the next event batch is processed. Drives never
  *    reorder past already-scheduled events.
  *  - Reads: `get(port)` returns the most recently driven value for the
  *    port, falling back to the value of its wire group, else None.
  *  - Delays: a port change is visible on its wire group `wireDelay` ticks
  *    later; a NAND gate's output reflects its inputs `gateDelay` ticks
  *    later (see [[Config]]).
  *  - Observation: `observe` reports every change of a port's effective
  *    value (as read by `get`) at least once, with the tick at which the new
  *    value became effective, in non-decreasing tick order. Redundant
  *    reports may occur; consumers such as [[LiveSim]] deduplicate.
  *
  * An implementation satisfying this contract can replace any other without
  * observable difference. That is the bar a new backend must clear to be
  * considered equivalent to the reference simulator.
  */
trait SimEngine {

  /** The current simulation time. Monotonic non-decreasing. */
  def tick: Long

  /** The port's effective value: the last driven value, else its wire-group
    * value, else None.
    */
  def get(port: Port): Option[Boolean]

  /** Schedule a drive of the port, applied at the current tick before the
    * next event batch is processed.
    */
  def set(port: Port, value: Option[Boolean]): SimEngine

  /** Register an external observer of the port. The callback is invoked from
    * the engine's own thread as the simulation advances; it must return
    * quickly and must not call back into the engine.
    *
    * Unlike [[Sim.watch]], the callback cannot modify the simulation — it is
    * purely for observation. Every effective-value change is reported at
    * least once, in non-decreasing tick order; redundant reports may occur.
    */
  def observe(port: Port, f: PortUpdate => Unit): SimEngine

  /** Advance the simulation, processing every event batch with timestamp <=
    * `deadline`, in timestamp order. Batches with timestamp > `deadline` are
    * not processed. After returning, `tick` is the timestamp of the last
    * processed batch, or unchanged if no batch had timestamp <= `deadline`.
    */
  def runTo(deadline: Long): SimEngine

  /** Process the next scheduled event batch (all events at the smallest
    * timestamp with pending events). If no events are scheduled, return
    * unchanged.
    */
  def step(): SimEngine
}

object SimEngine {

  /** The reference implementation: the event-driven [[Sim]]. */
  def reference(circuit: Circuit, conf: Config = Config.default): SimEngine =
    SimSetup.setup(circuit, conf)
}
