package simulator

import core.*

/** The reference live simulator: a thread-safe [[Sim]] running full gate-level simulation, paced against the wall
  * clock.
  *
  * A RefSim is a thin factory over [[LiveSim]]: it builds a `LiveSim` over the [[GateProcessor]] engine (via the
  * [[GateEngine]] adapter) and delegates every [[Sim]] method to it. All of the live, concurrent machinery — the inbox,
  * the paced loop, the published state, the notifier — lives in `LiveSim` and is shared with future compiled engines;
  * `RefSim` only chooses the gate-level one.
  *
  * A RefSim runs a [[GateProcessor]] — the functional discrete-event engine — in real time:
  *
  *   - [[set]]/[[unset]] drive ports from any thread; drives are queued together with their offer time and applied by
  *     the simulation thread at the sim tick corresponding to that time, so a wall-clock separation between two drives
  *     is preserved in sim time even if the simulation thread was starved in between. A drive is never applied in the
  *     past: a stale offer time falls back to the current tick.
  *   - [[subscribe]]/[[watch]] observe effective-value changes; each change is delivered exactly once, in simulation
  *     order.
  *   - [[start]] runs the simulation paced at `ticksPerSecond` simulation ticks per second of wall-clock time; [[stop]]
  *     halts it. The loop never stops on its own: it runs until `stop` is called.
  *
  * Peripherals interact only through the [[Sim]] interface: they see ports, never ticks — if the simulation lags behind
  * the wall clock, peripherals lag in kind, exactly like real hardware.
  *
  * The simulation thread never blocks on user code: drives are queued, `get` reads the last published state without
  * locking, and observer callbacks run on a separate single-threaded notifier. All threads are daemon threads.
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
) extends Sim {

  private val live = LiveSim(() => GateEngine(GateProcessor.setup(circuit, conf)), ticksPerSecond)

  /** Start the paced simulation loop. Throws IllegalStateException if already running.
    */
  def start(): Unit = live.start()

  /** Ask the paced loop to stop. Safe to call when not running. */
  def stop(): Unit = live.stop()

  def isRunning: Boolean = live.isRunning

  /** Drive the port; the drive is queued with its offer time and applied by the simulation thread at the corresponding
    * sim tick when the inbox is next drained.
    */
  def set(port: Port, value: Option[Boolean]): Unit = live.set(port, value)

  /** Drive every port of the bus. All drives share one offer time and are applied together by the simulation thread at
    * the corresponding sim tick when the inbox is next drained.
    */
  def set(bus: Bus, values: Seq[Boolean]): Unit = live.set(bus, values)

  /** The port's effective value in the last published state. */
  def get(port: Port): Option[Boolean] = live.get(port)

  def get(bus: Bus): Vector[Option[Boolean]] = live.get(bus)

  /** Run `callback` on every effective-value change of the port. Callbacks run sequentially on the notifier thread, in
    * simulation order. A callback must return quickly and must not throw (exceptions are printed and ignored). It may
    * drive inputs with [[set]]/[[unset]], but must not call [[start]] or [[stop]], which control the run loop.
    */
  def watch(port: Port)(callback: PortUpdate => Unit): AutoCloseable = live.watch(port)(callback)

  /** Subscribe to effective-value changes of the port. Each change is delivered exactly once, in simulation order. Poll
    * the returned subscription from any thread.
    */
  def subscribe(port: Port): PollSubscription = live.subscribe(port)
}

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
