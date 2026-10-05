package simulator

import java.util.concurrent.{Executors, ThreadFactory, TimeUnit}

import scala.concurrent.{ExecutionContext, Future, Promise}

import core.*

/** Synchronization primitives for peripherals driving a live [[Sim]].
  *
  * A peripheral can only drive wires, read wires, and react to wire changes — the same interface real hardware exposes.
  * These helpers turn wire observations into Scala `Future`s, so a peripheral can sequence its protocol without
  * arbitrary wall-clock sleeps and without any knowledge of the simulator's internals (no ticks, no stepping, no "has
  * the event queue drained" queries, none of which a real peripheral could ask).
  *
  * Race safety: [[awaitCondition]] reads the published state, installs watchers, then reads the published state again.
  * The second read catches any change published before the watcher installation completes; changes processed after
  * installation are reported by the watchers themselves. The remaining gap — a change processed before installation but
  * published after the second read — is closed by the simulator: [[RefSim]] reports the port's current value when the
  * watcher is installed if that value was never dispatched, so no effective-value change the condition depends on can
  * slip through unnoticed. Every wait therefore observes every relevant change exactly once.
  */
object Sync {

  private def daemonThreads(name: String): ThreadFactory =
    (r: Runnable) => {
      val t = new Thread(r, name)
      t.setDaemon(true)
      t
    }

  private val scheduler = Executors.newSingleThreadScheduledExecutor(daemonThreads("sim-sync-after"))

  /** A Future completing after `ms` wall-clock milliseconds. Hardware-style phase timing: a real peripheral holds a
    * level for a minimum pulse width with a timer; this is that timer.
    */
  def after(ms: Long)(using ExecutionContext): Future[Unit] = {
    val p = Promise[Unit]()
    scheduler.schedule((() => p.trySuccess(())): Runnable, ms, TimeUnit.MILLISECONDS)
    p.future
  }

  /** A Future completing once the port's effective value equals `target`. */
  def awaitPort(sim: Sim, port: Port, target: Option[Boolean])(using ExecutionContext): Future[Unit] =
    awaitCondition(sim, Seq(port))(sim.get(port) == target)

  /** A Future completing once the bus's effective value equals `target`. */
  def awaitBus(sim: Sim, bus: Bus, target: Vector[Option[Boolean]])(using ExecutionContext): Future[Unit] =
    awaitCondition(sim, bus)(sim.get(bus) == target)

  /** A Future completing once `cond` — read from the sim's published state — holds. Watchers are installed on
    * `watchPorts`: every port whose changes can affect `cond` must be listed, so no relevant change goes unnoticed.
    */
  def awaitCondition(sim: Sim, watchPorts: Seq[Port])(cond: => Boolean)(using ExecutionContext): Future[Unit] = {
    def matches(): Boolean = cond
    if (matches()) Future.successful(())
    else {
      val p = Promise[Unit]()
      val subs = watchPorts.map(port =>
        sim.watch(port) { _ =>
          if (matches()) p.trySuccess(()); ()
        }
      )
      // Unregister once the outcome is known, whichever thread decides it.
      p.future.onComplete(_ => subs.foreach(_.close()))(ExecutionContext.parasitic)
      // Second read: closes the race between the first read and the watcher installation.
      if (matches()) p.trySuccess(())
      p.future
    }
  }
}
