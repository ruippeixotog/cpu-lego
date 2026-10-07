package simulator

import core.*

/** A read-only, immutable view of an [[Engine]]'s state.
  *
  * A snapshot is detached from the engine: reading it never observes a half-advanced simulation, and it is safe to
  * share with reader threads.
  */
trait EngineState {

  /** The port's effective value: the last driven value, else its wire-group value, else None. */
  def get(port: Port): Option[Boolean]

  /** The effective value of every port of the bus. */
  def get(bus: Bus): Vector[Option[Boolean]]
}

/** A deterministic, single-threaded discrete-event simulation engine.
  *
  * Not thread-safe: an engine is driven from one thread at a time. [[LiveSim]] paces it with `tick` and `runTo`; `step`
  * is for tests and diagnostics. [[Sim]] exposes none of them.
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

  /** An immutable view of the engine's current state, safe to publish to reader threads. */
  def snapshot: EngineState

  /** Run `callback` with the port's new effective value on every change, in simulation order. The callback runs on the
    * simulation thread while the engine is mid-step; it must not drive the engine or block — use it only to observe.
    */
  def watch(port: Port)(callback: Option[Boolean] => Unit): Unit
}
