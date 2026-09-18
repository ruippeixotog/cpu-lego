package simulator

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference

import core.*

/** A thread-safe live handle on a running simulation.
  *
  * A LiveSim wraps a [[SimEngine]] — any backend implementing the engine
  * interface — and lets arbitrary code outside the simulator drive inputs
  * and observe outputs in real time:
  *
  *  - [[set]]/[[unset]] drive ports from any thread; drives are queued and
  *    applied by the simulation thread when the inbox is drained (at the
  *    start of each [[step]], [[runUntil]], or realtime-loop quantum).
  *  - [[subscribe]]/[[watch]] observe tick-stamped effective-value changes;
  *    each change is delivered exactly once, in tick order.
  *  - [[step]] advances the simulation by one event batch on the calling
  *    thread; [[runUntil]] advances it to a tick; [[runRealtime]]/[[startRealtime]]
  *    pace it against the wall clock at a fixed ticks-per-second rate.
  *
  * The simulation thread never blocks on user code: drives are queued,
  * `get`/`tick` read the last published state without locking, and observer
  * callbacks run on a separate single-threaded notifier.
  *
  * Only one run loop ([[step]], [[runUntil]], [[runRealtime]] or [[startRealtime]])
  * may be active at a time; starting a second one throws
  * IllegalStateException. All threads used are daemon threads.
  */
final class LiveSim private (initialEngine: SimEngine) {

  private type Action = SimEngine => SimEngine

  private val engine = new AtomicReference[SimEngine](initialEngine)
  private val inbox = new LinkedBlockingQueue[Action]()
  private val loopGuard = new AtomicBoolean(false)
  private val running = new AtomicBoolean(false)

  // How long the realtime loop sleeps between quanta when it is ahead of
  // the wall clock. Short enough to pick up drives promptly.
  private val QuantumMillis = 5

  private val observedPorts = ConcurrentHashMap.newKeySet[Port]()
  private val lastNotified = new ConcurrentHashMap[Port, Option[Boolean]]()
  private val subscribers = new ConcurrentHashMap[Port, CopyOnWriteArrayList[Subscriber]]()

  private val notifications = new LinkedBlockingQueue[() => Unit]()
  private lazy val notifierThread: Thread = {
    val t = new Thread(
      () => {
        var go = true
        while (go) {
          try {
            val item = notifications.take()
            item()
          } catch {
            case _: InterruptedException => go = false
          }
        }
      },
      "livesim-notifier"
    )
    t.setDaemon(true)
    t.start()
    t
  }

  // --- reads ---

  /** The last published simulation time. */
  def tick: Long = engine.get().tick

  /** The port's effective value in the last published state. */
  def get(port: Port): Option[Boolean] = engine.get().get(port)

  // --- drives: callable from any thread ---

  /** Drive the port; the drive is queued and applied by the simulation
    * thread when the inbox is next drained.
    */
  def set(port: Port, value: Boolean): Unit =
    inbox.offer(_.set(port, Some(value)))

  /** Drive the port; the drive is queued and applied by the simulation
    * thread when the inbox is next drained.
    */
  def set(port: Port, value: Option[Boolean]): Unit =
    inbox.offer(_.set(port, value))

  /** Release the port; the drive is queued like [[set]]. */
  def unset(port: Port): Unit = set(port, None)

  /** Drive every port of the bus. All drives are applied together by the
    * simulation thread when the inbox is next drained.
    */
  def set(bus: Bus, values: Seq[Boolean]): Unit =
    inbox.offer(e => bus.zip(values).foldLeft(e) { case (e1, (p, v)) => e1.set(p, Some(v)) })

  // --- observation ---

  /** Subscribe to tick-stamped effective-value changes of the port.
    * Each change is delivered exactly once, in tick order. Poll the
    * returned subscription from any thread.
    */
  def subscribe(port: Port): Subscription = {
    ensureObserved(port)
    val sub = new PollSubscriber
    subscribers.computeIfAbsent(port, _ => new CopyOnWriteArrayList[Subscriber]()).add(sub)
    new Subscription(port, sub)
  }

  /** Run `callback` on every tick-stamped effective-value change of the
    * port. Callbacks run sequentially on the notifier thread, in tick
    * order. A callback must return quickly and must not throw (exceptions
    * are printed and ignored). It may drive inputs with [[set]]/[[unset]]
    * — the drives are queued like any other — but must not call [[step]],
    * [[runUntil]], [[runRealtime]], [[startRealtime]] or [[stop]], which
    * control the run loop.
    */
  def watch(port: Port)(callback: PortUpdate => Unit): AutoCloseable = {
    ensureObserved(port)
    val sub = new CallbackSubscriber(callback)
    subscribers.computeIfAbsent(port, _ => new CopyOnWriteArrayList[Subscriber]()).add(sub)
    () => unsubscribe(port, sub)
  }

  private def ensureObserved(port: Port): Unit =
    if (observedPorts.add(port))
      inbox.offer(_.observe(port, dispatch))

  /** Deduplicate the engine's observer reports down to exactly one
    * notification per effective-value change, then fan out to subscribers.
    * Runs on the engine's thread; only touches concurrent structures.
    */
  private def dispatch(u: PortUpdate): Unit = {
    if (lastNotified.get(u.port) != u.value) {
      lastNotified.put(u.port, u.value)
      val subs = subscribers.get(u.port)
      if (subs != null) subs.forEach(_.deliver(u))
    }
  }

  private def unsubscribe(port: Port, sub: Subscriber): Unit = {
    val subs = subscribers.get(port)
    if (subs != null) subs.remove(sub)
  }

  // --- running ---

  /** Process the next scheduled event batch on the calling thread.
    * Drives queued before this call are applied first; drives queued
    * during it wait for the next call.
    */
  def step(): Unit =
    withLoop {
      engine.set(drain(engine.get()).step())
    }

  /** Advance the simulation synchronously on the calling thread until
    * `tick >= tick` or the engine is idle. Drives queued before this call
    * are applied first; drives queued during it wait for the next call.
    */
  def runUntil(tick: Long): Unit =
    withLoop {
      engine.set(drain(engine.get()).runTo(tick))
    }

  /** Pace the simulation against the wall clock at `ticksPerSecond`
    * simulation ticks per second. Blocks until [[stop]] is called.
    */
  def runRealtime(ticksPerSecond: Long): Unit = {
    require(ticksPerSecond > 0, "ticksPerSecond must be positive")
    withLoop(realtimeLoop(ticksPerSecond))
  }

  /** Start [[runRealtime]] on a daemon thread and return the thread. */
  def startRealtime(ticksPerSecond: Long): Thread = {
    require(ticksPerSecond > 0, "ticksPerSecond must be positive")
    if (!loopGuard.compareAndSet(false, true))
      throw new IllegalStateException("a LiveSim run loop is already active")
    val t = new Thread(
      () => {
        try realtimeLoop(ticksPerSecond)
        finally loopGuard.set(false)
      },
      "livesim-realtime"
    )
    t.setDaemon(true)
    t.start()
    t
  }

  /** Ask a running realtime loop to stop. Safe to call when no loop is
    * running.
    */
  def stop(): Unit = {
    running.set(false)
    inbox.offer(identity) // wake the loop if it is sleeping in poll
  }

  private def realtimeLoop(ticksPerSecond: Long): Unit = {
    running.set(true)
    try {
      val startTick = engine.get().tick
      val startNanos = System.nanoTime()
      while (running.get()) {
        val deadline = startTick + saturatingTicks(System.nanoTime() - startNanos, ticksPerSecond)
        engine.set(drain(engine.get()).runTo(deadline))
        try {
          val a = inbox.poll(QuantumMillis, TimeUnit.MILLISECONDS)
          if (a != null) engine.set(a(engine.get()))
        } catch {
          case _: InterruptedException =>
            running.set(false)
            Thread.currentThread().interrupt()
        }
      }
    } finally running.set(false)
  }

  private def drain(e0: SimEngine): SimEngine = {
    var e = e0
    var a = inbox.poll()
    while (a != null) {
      e = a(e)
      a = inbox.poll()
    }
    e
  }

  private def withLoop(body: => Unit): Unit =
    if (loopGuard.compareAndSet(false, true)) {
      try body
      finally loopGuard.set(false)
    } else throw new IllegalStateException("a LiveSim run loop is already active")

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

  private final class PollSubscriber extends Subscriber {
    val queue = new LinkedBlockingQueue[PortUpdate]()
    def deliver(u: PortUpdate): Unit = queue.offer(u)
  }

  private final class CallbackSubscriber(callback: PortUpdate => Unit) extends Subscriber {
    def deliver(u: PortUpdate): Unit = {
      notifierThread // force the notifier thread to start
      notifications.offer { () =>
        try callback(u)
        catch { case e: Exception => e.printStackTrace() }
      }
      ()
    }
  }

  /** A pollable subscription to a port's value changes. */
  final class Subscription private[LiveSim] (val port: Port, sub: PollSubscriber) extends AutoCloseable {

    /** All updates delivered since the last poll, in tick order. */
    def poll(): List[PortUpdate] = {
      val b = List.newBuilder[PortUpdate]
      var u = sub.queue.poll()
      while (u != null) {
        b += u
        u = sub.queue.poll()
      }
      b.result()
    }

    /** Unsubscribe. Updates already queued can still be polled. */
    def close(): Unit = unsubscribe(port, sub)
  }
}

object LiveSim {

  /** Wrap any [[SimEngine]] implementation. */
  def apply(engine: SimEngine): LiveSim = new LiveSim(engine)

  /** Build a LiveSim on the reference engine for a circuit.
    *
    * A custom backend is used as `LiveSim(MyBackend(circuit, conf))`, where
    * `MyBackend(circuit: Circuit, conf: Config): SimEngine` builds the
    * backend from the gate graph; its internals are opaque.
    */
  def apply(
      circuit: Circuit,
      conf: Config = Config.default
  ): LiveSim =
    new LiveSim(SimEngine.reference(circuit, conf))
}
