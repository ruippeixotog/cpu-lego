package simulator

import java.util.concurrent.LinkedBlockingQueue

import core.*

/** A change in a port's effective value, as observed on a live [[Sim]].
  *
  * Deliberately not tick-stamped: peripherals see ports, never the simulator's clock — if the simulation lags behind
  * the wall clock, peripherals lag in kind, exactly like real hardware.
  *
  * @param port
  *   the port whose value changed
  * @param value
  *   the port's new effective value, as read by `get`
  */
final case class PortUpdate(port: Port, value: Option[Boolean])

/** A live simulator: a running simulation that peripherals drive and observe.
  *
  * A Sim is the seam between the outside world and any simulation implementation: the reference gate-level [[RefSim]],
  * a future backend that compiles the gate graph to native code, an FPGA driver, and so on. All interaction with a
  * running simulation goes through this interface; what an implementation does with the gates internally is opaque.
  *
  * The contract is deliberately small, modeling what a real peripheral can do: drive wires, read wires, and react to
  * wire changes. There is no tick and no stepping — a peripheral cannot ask a real CPU for its current cycle, and
  * neither can it ask a Sim.
  *
  * All methods are safe to call from any thread. Implementations must document their execution model: when the
  * simulation advances, whether it can stop on its own, and what `start`/`stop` mean.
  */
trait Sim {

  /** Start the simulation. Throws IllegalStateException if already running.
    */
  def start(): Unit

  /** Ask a running simulation to stop. Safe to call when not running. */
  def stop(): Unit

  /** Whether the simulation is currently running. */
  def isRunning: Boolean

  /** Drive the port to `value`; `None` releases it. */
  def set(port: Port, value: Option[Boolean]): Unit

  /** Drive the port to `value`. */
  def set(port: Port, value: Boolean): Unit = set(port, Some(value))

  /** Release the port (equivalent to driving `None`). */
  def unset(port: Port): Unit = set(port, None)

  /** Drive every port of the bus. All drives are applied together.
    */
  def set(bus: Bus, values: Seq[Boolean]): Unit

  /** The port's effective value in the last published state: the last driven value, else its wire-group value, else
    * None.
    */
  def get(port: Port): Option[Boolean]

  /** The effective value of every port of the bus. */
  def get(bus: Bus): Vector[Option[Boolean]]

  /** Run `callback` on every effective-value change of the port. Callbacks run sequentially on a notifier thread, in
    * simulation order. A callback must return quickly and must not throw (exceptions are reported and ignored). It may
    * drive inputs with [[set]]/[[unset]], but must not call [[start]] or [[stop]].
    *
    * @return
    *   an AutoCloseable that unregisters the callback
    */
  def watch(port: Port)(callback: PortUpdate => Unit): AutoCloseable

  /** Subscribe to effective-value changes of the port. Each change is delivered exactly once, in simulation order. Poll
    * the returned subscription from any thread.
    */
  def subscribe(port: Port): PollSubscription
}

/** A pollable subscription to a port's value changes, as returned by [[Sim.subscribe]].
  */
final class PollSubscription private[simulator] (
    val port: Port,
    private val queue: LinkedBlockingQueue[PortUpdate],
    private val onClose: () => Unit
) extends AutoCloseable {

  /** All updates delivered since the last poll, in delivery order. */
  def poll(): List[PortUpdate] = {
    val b = List.newBuilder[PortUpdate]
    var u = queue.poll()
    while (u != null) {
      b += u
      u = queue.poll()
    }
    b.result()
  }

  /** Unsubscribe. Updates already queued can still be polled. */
  def close(): Unit = onClose()
}
