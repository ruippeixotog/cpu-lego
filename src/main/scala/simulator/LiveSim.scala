package simulator

import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicReference}
import java.util.concurrent.{CopyOnWriteArrayList, Executors, LinkedBlockingQueue, ThreadFactory, TimeUnit}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

import core.*

/** A live simulator over any [[Engine]]: a thread-safe [[Sim]] running a deterministic simulation, paced against the
  * wall clock.
  *
  * Owns the concurrent machinery (inbox, paced loop, published state, notifier) around a single-threaded [[Engine]];
  * [[RefSim]] runs it over the gate-level engine.
  *
  * A compiled backend reuses all of this by supplying its own `Engine`; the reference gate-level simulator is just
  * `LiveSim(() => GateEngine(GateProcessor.setup(circuit, conf)), ticksPerSecond)` (see [[RefSim]]).
  *
  * Threading model:
  *
  *   - The engine is confined to the simulation thread. The `newEngine` factory is invoked once, on the constructing
  *     thread; every later `Engine` call — drives, `runTo`, `watch` — happens on the simulation thread. Engine
  *     implementations need no internal synchronization.
  *   - The published state is an `AtomicReference[EngineState]`. After each pacing quantum (and after applying a polled
  *     inbox entry) the simulation thread publishes an immutable snapshot; that volatile write is the happens-before
  *     edge that makes the snapshot safely visible to reader threads. A reader always sees a complete, consistent
  *     engine state, never a half-advanced one.
  *   - Drives are queued, `get` reads the last published state without locking, and observer callbacks run on a
  *     separate single-threaded notifier. All threads are daemon threads.
  *
  * Its guarantees:
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
  * @param newEngine
  *   builds the simulation engine; invoked once, at construction
  * @param ticksPerSecond
  *   simulation ticks per wall-clock second while running
  */
class LiveSim(newEngine: () => Engine, ticksPerSecond: Long = 1000) extends Sim {
  require(ticksPerSecond > 0, "ticksPerSecond must be positive")

  private type Action = Engine => Unit

  /** An inbox entry: the action plus the wall-clock instant it was offered. Drives carry their offer time so they can
    * be applied at the corresponding sim tick; entries with no drive-timing meaning (the stop wake-up, observer
    * installs) carry `None` and apply at the current tick.
    */
  private case class InboxEntry(action: Action, offerNanos: Option[Long])

  // The engine itself, touched only by the simulation thread. Readers never see it: they read the immutable snapshots
  // published below.
  private val engine: Engine = newEngine()
  private val published = new AtomicReference[EngineState](engine.snapshot)
  private val inbox = new LinkedBlockingQueue[InboxEntry]()
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
    } else throw new IllegalStateException("LiveSim is already running")

  /** Ask the paced loop to stop. Safe to call when not running. */
  def stop(): Unit = {
    running.set(false)
    inbox.offer(InboxEntry(_ => (), None)) // wake the loop if it is sleeping in poll
  }

  def isRunning: Boolean = running.get()

  // --- drives: callable from any thread ---

  /** Drive the port; the drive is queued with its offer time and applied by the simulation thread at the corresponding
    * sim tick when the inbox is next drained.
    */
  def set(port: Port, value: Option[Boolean]): Unit =
    inbox.offer(InboxEntry(_.set(port, value), Some(System.nanoTime())))

  /** Drive every port of the bus. All drives share one offer time and are applied together by the simulation thread at
    * the corresponding sim tick when the inbox is next drained.
    */
  def set(bus: Bus, values: Seq[Boolean]): Unit =
    inbox.offer(InboxEntry(_.set(bus, values), Some(System.nanoTime())))

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
    // Register before requesting observation: the install action (and its catch-up) runs after the request is
    // offered, so a notification can never be dispatched before the new subscriber is present to receive it.
    val sub = new CallbackSubscriber(callback)
    subscribers.getOrElseUpdate(port, new CopyOnWriteArrayList[Subscriber]()).add(sub)
    ensureObserved(port)
    () => unsubscribe(port, sub)
  }

  /** Subscribe to effective-value changes of the port. Each change is delivered exactly once, in simulation order. Poll
    * the returned subscription from any thread.
    */
  def subscribe(port: Port): PollSubscription = {
    // Register before requesting observation: see watch.
    val queue = new LinkedBlockingQueue[PortUpdate]()
    val sub = new PollSubscriber(queue)
    subscribers.getOrElseUpdate(port, new CopyOnWriteArrayList[Subscriber]()).add(sub)
    ensureObserved(port)
    new PollSubscription(port, queue, () => unsubscribe(port, sub))
  }

  private def ensureObserved(port: Port): Unit =
    if (observedPorts.putIfAbsent(port, ()).isEmpty)
      inbox.offer(
        InboxEntry(
          e => {
            val current = e.get(port)
            // Catch-up: the port's last change may have been processed before this first observer was installed, in
            // which case no callback would ever report it — yet it is already visible in the published state. Report the
            // current value if it was never dispatched, so every effective-value change is delivered exactly once even
            // when it predates the first watcher. This closes the observer-installation race: with the catch-up, a change
            // is either reported here (processed before install) or by the observer below (processed after install).
            if (current.isDefined && lastNotified.get(port) != Some(current))
              pendingNotifications += PortUpdate(port, current)
            e.watch(port)(v => pendingNotifications += PortUpdate(port, v))
          },
          None
        )
      )

  /** Port changes observed during the current pacing quantum, in simulation order. Buffered on the simulation thread
    * and dispatched only after the new state is published, so an observer that reads back through [[get]] always sees
    * the change it is being notified about — never the previous state.
    */
  private val pendingNotifications = ListBuffer.empty[PortUpdate]

  /** Dispatch the quantum's buffered notifications, in order. Runs on the simulation thread, after publishing. */
  private def dispatchNotifications(): Unit = {
    val updates = pendingNotifications.toList
    pendingNotifications.clear()
    updates.foreach(dispatch)
  }

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
    val startTick = engine.tick
    val startNanos = System.nanoTime()
    while (running.get() && generation.get() == gen) {
      val deadline = startTick + saturatingTicks(System.nanoTime() - startNanos, ticksPerSecond)
      drain(startTick, startNanos)
      engine.runTo(deadline)
      published.set(engine.snapshot)
      dispatchNotifications()
      try {
        val entry = inbox.poll(QuantumMillis, TimeUnit.MILLISECONDS)
        if (entry != null) {
          applyEntry(entry, startTick, startNanos)
          published.set(engine.snapshot)
        }
      } catch {
        case _: InterruptedException => running.set(false)
      }
    }
  }

  private def drain(startTick: Long, startNanos: Long): Unit = {
    var entry = inbox.poll()
    while (entry != null) {
      applyEntry(entry, startTick, startNanos)
      entry = inbox.poll()
    }
  }

  /** Apply one inbox entry. A stamped drive is advanced to its offer tick first — the sim time corresponding to the
    * wall-clock instant it was offered — so drives land in sim time when they were driven in wall-clock time, even if
    * the simulation thread was starved while they queued. The `max` keeps the engine from ever moving backwards: a
    * stale stamp (an offer predating this run's pacing anchor, or a nanosecond race where the loop iterated between the
    * `set` call and the enqueue) falls back to applying at the current tick, exactly the case where the simulation is
    * not starved anyway. Unstamped entries apply at the current tick.
    */
  private def applyEntry(entry: InboxEntry, startTick: Long, startNanos: Long): Unit = {
    entry.offerNanos match {
      case Some(nanos) =>
        val stampTick = startTick + saturatingTicks(nanos - startNanos, ticksPerSecond)
        if (stampTick > engine.tick) engine.runTo(stampTick)
      case None => ()
    }
    entry.action(engine)
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
