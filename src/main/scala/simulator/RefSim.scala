package simulator

import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.Executors
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.ThreadFactory
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicReference

import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import core.*

/** The reference live simulator: a thread-safe [[Sim]] running full gate-level simulation, paced against the wall
  * clock.
  *
  * A RefSim runs a [[GateProcessor]] — the functional discrete-event engine — in real time:
  *
  *   - [[set]]/[[unset]] drive ports from any thread; drives are queued and applied by the simulation thread when the
  *     inbox is drained at the start of each pacing quantum.
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
  require(ticksPerSecond > 0, "ticksPerSecond must be positive")

  private type Action = GateProcessor => GateProcessor

  private val published = new AtomicReference[GateProcessor](SimSetup.setup(circuit, conf))
  private val inbox = new LinkedBlockingQueue[Action]()
  private val running = new AtomicBoolean(false)
  private val generation = new AtomicLong(0)

  // How long the paced loop sleeps between quanta when it is ahead of the
  // wall clock. Short enough to pick up drives promptly.
  private val QuantumMillis = 5

  private val observedPorts = TrieMap.empty[Port, Unit]
  private val lastNotified = TrieMap.empty[Port, Option[Boolean]]
  private val subscribers = TrieMap.empty[Port, CopyOnWriteArrayList[Subscriber]]

  private def daemonThreads(name: String): ThreadFactory =
    (r: Runnable) => {
      val t = new Thread(r, name)
      t.setDaemon(true)
      t
    }

  private val simEc: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor(daemonThreads("refsim-loop")))
  private val notifierEc: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor(daemonThreads("refsim-notifier")))

  // --- lifecycle ---

  /** Start the paced simulation loop. Throws IllegalStateException if already running.
    */
  def start(): Unit =
    if (running.compareAndSet(false, true)) {
      val gen = generation.incrementAndGet()
      Future {
        try realtimeLoop(gen)
        finally if (generation.get() == gen) running.set(false)
      }(simEc).failed.foreach(_.printStackTrace())(ExecutionContext.parasitic)
    } else throw new IllegalStateException("RefSim is already running")

  /** Ask the paced loop to stop. Safe to call when not running. */
  def stop(): Unit = {
    running.set(false)
    inbox.offer(identity) // wake the loop if it is sleeping in poll
  }

  def isRunning: Boolean = running.get()

  // --- drives: callable from any thread ---

  /** Drive the port; the drive is queued and applied by the simulation thread when the inbox is next drained.
    */
  def set(port: Port, value: Option[Boolean]): Unit =
    inbox.offer(_.set(port, value))

  /** Drive every port of the bus. All drives are applied together by the simulation thread when the inbox is next
    * drained.
    */
  def set(bus: Bus, values: Seq[Boolean]): Unit =
    inbox.offer(p => bus.zip(values).foldLeft(p) { case (p1, (port, v)) => p1.set(port, Some(v)) })

  // --- reads ---

  /** The port's effective value in the last published state. */
  def get(port: Port): Option[Boolean] = published.get().get(port)

  def get(bus: Bus): Vector[Option[Boolean]] = published.get().get(bus)

  // --- observation ---

  /** Run `callback` on every effective-value change of the port. Callbacks run sequentially on the notifier thread, in
    * simulation order. A callback must return quickly and must not throw (exceptions are printed and ignored). It may
    * drive inputs with [[set]]/[[unset]], but must not call [[start]] or [[stop]], which control the run loop.
    */
  def watch(port: Port)(callback: PortUpdate => Unit): AutoCloseable = {
    ensureObserved(port)
    val sub = new CallbackSubscriber(callback)
    subscribers.getOrElseUpdate(port, new CopyOnWriteArrayList[Subscriber]()).add(sub)
    () => unsubscribe(port, sub)
  }

  /** Subscribe to effective-value changes of the port. Each change is delivered exactly once, in simulation order. Poll
    * the returned subscription from any thread.
    */
  def subscribe(port: Port): PollSubscription = {
    ensureObserved(port)
    val queue = new LinkedBlockingQueue[PortUpdate]()
    val sub = new PollSubscriber(queue)
    subscribers.getOrElseUpdate(port, new CopyOnWriteArrayList[Subscriber]()).add(sub)
    new PollSubscription(port, queue, () => unsubscribe(port, sub))
  }

  private def ensureObserved(port: Port): Unit =
    if (observedPorts.putIfAbsent(port, ()).isEmpty)
      inbox.offer(_.watch(port)(s => { dispatch(PortUpdate(port, s.get(port))); s }))

  /** Deduplicate the processor's observer reports down to exactly one notification per effective-value change, then fan
    * out to subscribers. Runs on the simulation thread; only touches concurrent structures.
    */
  private def dispatch(u: PortUpdate): Unit =
    if (lastNotified.get(u.port) != Some(u.value)) {
      lastNotified.put(u.port, u.value)
      subscribers.get(u.port).foreach(_.forEach(_.deliver(u)))
    }

  private def unsubscribe(port: Port, sub: Subscriber): Unit =
    subscribers.get(port).foreach(_.remove(sub))

  // --- paced loop ---

  private def realtimeLoop(gen: Long): Unit = {
    val startTick = published.get().tick
    val startNanos = System.nanoTime()
    while (running.get() && generation.get() == gen) {
      val deadline = startTick + saturatingTicks(System.nanoTime() - startNanos, ticksPerSecond)
      published.set(drain(published.get()).runTo(deadline))
      try {
        val a = inbox.poll(QuantumMillis, TimeUnit.MILLISECONDS)
        if (a != null) published.set(a(published.get()))
      } catch {
        case _: InterruptedException => running.set(false)
      }
    }
  }

  private def drain(p0: GateProcessor): GateProcessor = {
    var p = p0
    var a = inbox.poll()
    while (a != null) {
      p = a(p)
      a = inbox.poll()
    }
    p
  }

  private def saturatingTicks(elapsedNanos: Long, ticksPerSecond: Long): Long = {
    val elapsedSeconds = elapsedNanos / 1000000000L
    if (elapsedSeconds > (Long.MaxValue - 1) / ticksPerSecond) Long.MaxValue
    else
      elapsedSeconds * ticksPerSecond +
        (elapsedNanos % 1000000000L) * ticksPerSecond / 1000000000L
  }

  // --- internals ---

  private sealed trait Subscriber {
    def deliver(u: PortUpdate): Unit
  }

  private final class PollSubscriber(queue: LinkedBlockingQueue[PortUpdate]) extends Subscriber {
    def deliver(u: PortUpdate): Unit = {
      queue.offer(u)
      ()
    }
  }

  private final class CallbackSubscriber(callback: PortUpdate => Unit) extends Subscriber {
    def deliver(u: PortUpdate): Unit = {
      Future {
        try callback(u)
        catch { case e: Exception => e.printStackTrace() }
      }(notifierEc)
      ()
    }
  }
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
