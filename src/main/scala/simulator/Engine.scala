package simulator

import core.*

/** A deterministic, single-threaded discrete-event simulation engine.
  *
  * An `Engine` holds the simulation state and advances it in place — the mutable counterpart of the functional engines
  * tests drive directly (see [[GateProcessor]]). Live simulators (see [[LiveSim]]) own exactly one engine and drive it
  * from their simulation thread, so an engine never needs internal synchronization.
  *
  * Threading model: an engine is confined to the thread that drives it. The factory passed to [[LiveSim]] is invoked
  * once, on the constructing thread; every later `Engine` call happens on the simulation thread. Implementations may
  * freely keep unsynchronized mutable state.
  *
  * `tick`, `step` and `runTo` are test/diagnostic-only: they let a future compiled engine be exercised
  * deterministically without a live simulator, but they are never exposed through [[Sim]] — a peripheral cannot ask
  * real hardware for its current cycle, and neither can it ask a Sim.
  */
trait Engine {

  /** Drive the port to `value`; `None` releases it. */
  def set(port: Port, value: Option[Boolean]): Unit

  /** Drive every port of the bus. */
  def set(bus: Bus, values: Seq[Boolean]): Unit

  /** The port's effective value: the last driven value, else its wire-group value, else None. */
  def get(port: Port): Option[Boolean]

  /** The effective value of every port of the bus. */
  def get(bus: Bus): Vector[Option[Boolean]]

  /** Advance the simulation to `deadline`, processing every event batch with timestamp <= `deadline`. */
  def runTo(deadline: Long): Unit

  /** Process the next scheduled event batch. If no events are scheduled, do nothing. */
  def step(): Unit

  /** The current simulation tick. */
  def tick: Long

  /** Run `callback` on every effective-value change of the port, in simulation order. The callback runs on the
    * simulation thread while the engine is mid-step; it must not drive the engine or block — use it only to observe.
    */
  def watch(port: Port)(callback: Engine => Unit): Unit
}

/** An [[Engine]] adapter over the functional [[GateProcessor]].
  *
  * The gate-level simulation itself stays purely functional: every `GateProcessor` operation returns a new processor,
  * and this adapter just swaps the current one in. Watch callbacks therefore always observe a fully-formed processor —
  * the adapter publishes the in-progress processor before invoking the callback, so `get` inside the callback sees the
  * change that triggered it.
  */
final class GateEngine(private var p: GateProcessor) extends Engine {

  def set(port: Port, value: Option[Boolean]): Unit = p = p.set(port, value)

  def set(bus: Bus, values: Seq[Boolean]): Unit = p = p.set(bus, values)

  def get(port: Port): Option[Boolean] = p.get(port)

  def get(bus: Bus): Vector[Option[Boolean]] = p.get(bus)

  def runTo(deadline: Long): Unit = p = p.runTo(deadline)

  def step(): Unit = p = p.step()

  def tick: Long = p.tick

  def watch(port: Port)(callback: Engine => Unit): Unit =
    p = p.watch(port) { q =>
      // The processor handed to the observer already reflects the port change; publish it before the callback reads.
      p = q
      callback(this)
      p
    }
}
