package simulator

import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicReference}
import java.util.concurrent.{CopyOnWriteArrayList, Executors, LinkedBlockingQueue, ThreadFactory, TimeUnit}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration.{Duration, FiniteDuration}

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
  *     halts it. With the default [[FaultPolicy.Halt]] the loop also stops on the first engine fault, completing
  *     [[failure]]; with [[FaultPolicy.Continue]] it runs until `stop` is called.
  *   - Bounded catch-up: if the simulation falls more than `maxCatchUp` of wall time behind — a stall (GC, debugger,
  *     laptop sleep) or an engine slower than real time — the pacing anchor is rebased onto the current tick and time,
  *     dropping the debt. The simulation resumes at the configured rate instead of fast-forwarding. Drive time-stamps
  *     stay consistent across a rebase: an offer time predating the new anchor is stale and applies at the current
  *     tick, as stale stamps do today.
  *
  * Host-side pacing diagnostics ([[achievedTicksPerSecond]], [[lagMs]], [[rebaseCount]]) live on this class, not on
  * [[Sim]]: peripherals must not observe simulator speed.
  *
  * Peripherals interact only through the [[Sim]] interface: they see ports, never ticks — if the simulation lags behind
  * the wall clock, peripherals lag in kind, exactly like real hardware.
  *
  * @param newEngine
  *   builds the simulation engine; invoked once, at construction
  * @param ticksPerSecond
  *   simulation ticks per wall-clock second while running; real-time pacing is `ticksPerSecond = 1_000_000_000` (1 tick =
  *   1 ns)
  * @param onFault
  *   what to do when the engine reports a fault: [[FaultPolicy.Halt]] (the default) stops the simulation loop on the
  *   first fault, [[FaultPolicy.Continue]] keeps simulating and only notifies [[faults]] subscribers
  * @param maxCatchUp
  *   how far behind the wall clock the simulation may fall before the pacing anchor is rebased, dropping the
  *   accumulated debt; must be positive
  */
class LiveSim(
    newEngine: () => Engine,
    ticksPerSecond: Long = 1000,
    onFault: FaultPolicy = FaultPolicy.Halt,
    val maxCatchUp: FiniteDuration = FiniteDuration(100, TimeUnit.MILLISECONDS),
    // Test seam: the clock the paced loop and drive offer-stamps are measured against. Production uses the wall clock.
    private[simulator] val nanoTime: () => Long = System.nanoTime
) extends Sim {
  require(ticksPerSecond > 0, "ticksPerSecond must be positive")
  require(maxCatchUp > Duration.Zero, "maxCatchUp must be positive")

  private val maxCatchUpMs = maxCatchUp.toUnit(TimeUnit.MILLISECONDS)

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

  // Pacing diagnostics, written by the simulation thread once per quantum, read from any thread. Deliberately not on
  // Sim: peripherals must not observe simulator speed.
  @volatile private var achievedTicksPerSecondValue = 0.0
  @volatile private var lagMsValue = 0.0
  private val rebaseCountValue = new AtomicLong(0)

  // How long the paced loop sleeps between quanta when it is ahead of the
  // wall clock. Short enough to pick up drives promptly.
  private val QuantumMillis = 5

  private val observedPorts = TrieMap.empty[Port, Unit]
  private val lastNotified = TrieMap.empty[Port, Option[Boolean]]
  private val subscribers = TrieMap.empty[Port, CopyOnWriteArrayList[Subscriber]]
  private val faultSubscribers = new CopyOnWriteArrayList[Fault => Unit]()
  private val failurePromise = Promise[Fault]()

  /** How many of the engine's recorded faults have been forwarded to subscribers. The engine only ever appends, so a
    * count is enough to pick out the fresh ones. Touched only by the simulation thread.
    */
  private var engineFaultsSeen = 0

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
    inbox.offer(InboxEntry(_.set(port, value), Some(nanoTime())))

  /** Drive every port of the bus. All drives share one offer time and are applied together by the simulation thread at
    * the corresponding sim tick when the inbox is next drained.
    */
  def set(bus: Ports, values: Seq[Boolean]): Unit =
    inbox.offer(InboxEntry(_.set(bus, values), Some(nanoTime())))

  // --- reads ---

  /** The port's effective value in the last published state. */
  def get(port: Port): Option[Boolean] = published.get().get(port)

  def get(bus: Ports): Vector[Option[Boolean]] = published.get().get(bus)

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

  /** Subscribe to structured faults reported by the simulation engine. Callbacks run sequentially on the notifier
    * thread, in recording order; a callback must return quickly and must not throw (exceptions are reported and
    * ignored).
    */
  def faults(callback: Fault => Unit): AutoCloseable = {
    faultSubscribers.add(callback)
    () => faultSubscribers.remove(callback)
  }

  /** A future completing with the first halting fault. With [[FaultPolicy.Halt]] (the default) the simulation stops on
    * the first fault and this completes with it; with [[FaultPolicy.Continue]] faults never halt the simulation, so
    * this never completes.
    */
  def failure: Future[Fault] = failurePromise.future

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

  // --- pacing diagnostics ---

  /** Achieved simulation ticks per wall-clock second, as an exponentially weighted moving average over ~1 second. For
    * host apps (HUD, benchmarks): deliberately not on [[Sim]] — a peripheral must not observe simulator speed.
    */
  def achievedTicksPerSecond: Double = achievedTicksPerSecondValue

  /** How far the simulation currently lags behind the wall clock, in milliseconds: wall time elapsed since the pacing
    * anchor minus sim time elapsed since the anchor. Bounded by [[maxCatchUp]]: when the lag would exceed it, the
    * pacing anchor is rebased instead. For host apps only — see [[achievedTicksPerSecond]].
    */
  def lagMs: Double = lagMsValue

  /** How many times the pacing anchor was rebased since construction (see [[maxCatchUp]]). For host apps only — see
    * [[achievedTicksPerSecond]].
    */
  def rebaseCount: Long = rebaseCountValue.get()

  // --- paced loop ---

  private def realtimeLoop(gen: Long): Unit = {
    var anchorTick = engine.tick
    var anchorNanos = nanoTime()
    var prevNanos = anchorNanos
    var prevTick = anchorTick
    var haveRateSample = false
    while (running.get() && generation.get() == gen) {
      val now = nanoTime()
      // How far the simulation lags behind the wall clock: wall time elapsed since the pacing anchor minus sim time
      // elapsed since the anchor. Normally ~zero; a stall (GC, debugger, laptop sleep) or an engine slower than real
      // time lets it grow. (Double: the tick-to-nanos product can exceed Long range over very long runs, and the
      // comparison against maxCatchUp needs none of that precision.)
      val debtMs = (now - anchorNanos) / 1000000.0 - (engine.tick - anchorTick) * 1000.0 / ticksPerSecond
      if (debtMs > maxCatchUpMs) {
        // The simulation fell more than maxCatchUp behind the wall clock. Rebase the pacing anchor onto the current
        // tick and time, dropping the debt: the simulation resumes at the configured rate from now instead of
        // fast-forwarding to catch up.
        anchorTick = engine.tick
        anchorNanos = now
        rebaseCountValue.incrementAndGet()
        lagMsValue = 0.0
      } else {
        // The engine cannot run past the pacing deadline, so the debt is never negative here; the max guards float
        // noise. Bounded by maxCatchUp: the rebase above fired if it would exceed it.
        lagMsValue = math.max(0.0, debtMs)
      }
      val deadline = anchorTick + saturatingTicks(now - anchorNanos, ticksPerSecond)
      drain(anchorTick, anchorNanos)
      engine.runTo(deadline)
      pollEngineFaults()
      // Pacing diagnostics, simulation thread only.
      val dtNanos = now - prevNanos
      val dTicks = engine.tick - prevTick
      if (dtNanos > 0) {
        val instant = dTicks.toDouble / dtNanos * 1000000000.0
        val alpha = math.min(1.0, dtNanos / 1000000000.0) // ~1 s EWMA window
        achievedTicksPerSecondValue =
          if (haveRateSample) achievedTicksPerSecondValue + alpha * (instant - achievedTicksPerSecondValue)
          else instant
        haveRateSample = true
      }
      prevNanos = now
      prevTick = engine.tick
      published.set(engine.snapshot)
      dispatchNotifications()
      try {
        val entry = inbox.poll(QuantumMillis, TimeUnit.MILLISECONDS)
        if (entry != null) {
          applyEntry(entry, anchorTick, anchorNanos)
          pollEngineFaults()
          published.set(engine.snapshot)
        }
      } catch {
        case _: InterruptedException => running.set(false)
      }
    }
  }

  /** Forward newly recorded engine faults to [[faults]] subscribers, in recording order. With [[FaultPolicy.Halt]] (the
    * default) the first fault also stops the simulation loop and completes [[failure]]. Runs on the simulation thread,
    * after the engine has advanced.
    */
  private def pollEngineFaults(): Unit = {
    val fresh = engine.faults.drop(engineFaultsSeen)
    if (fresh.nonEmpty) {
      engineFaultsSeen += fresh.size
      fresh.foreach(dispatchFault)
      if (onFault == FaultPolicy.Halt) {
        failurePromise.trySuccess(fresh.head)
        running.set(false)
      }
    }
  }

  /** Deliver one fault to every subscriber, on the notifier thread. A throwing callback is reported and ignored, like
    * watch callbacks. Runs on the simulation thread; only touches concurrent structures.
    */
  private def dispatchFault(f: Fault): Unit =
    faultSubscribers.forEach { cb =>
      Future {
        try cb(f)
        catch { case e: Exception => e.printStackTrace() }
      }(notifierEc)
      ()
    }

  private def drain(anchorTick: Long, anchorNanos: Long): Unit = {
    var entry = inbox.poll()
    while (entry != null) {
      applyEntry(entry, anchorTick, anchorNanos)
      entry = inbox.poll()
    }
  }

  /** Apply one inbox entry. A stamped drive is advanced to its offer tick first — the sim time corresponding to the
    * wall-clock instant it was offered — so drives land in sim time when they were driven in wall-clock time, even if
    * the simulation thread was starved while they queued. The `max` keeps the engine from ever moving backwards: a
    * stale stamp (an offer predating the pacing anchor — including one predating a rebase of the anchor — or a
    * nanosecond race where the loop iterated between the `set` call and the enqueue) falls back to applying at the
    * current tick, exactly the case where the simulation is not starved anyway. Unstamped entries apply at the current
    * tick.
    */
  private def applyEntry(entry: InboxEntry, anchorTick: Long, anchorNanos: Long): Unit = {
    entry.offerNanos match {
      case Some(nanos) =>
        val stampTick = anchorTick + saturatingTicks(nanos - anchorNanos, ticksPerSecond)
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
