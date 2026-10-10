package simulator

import java.util.concurrent.atomic.AtomicLong

import scala.concurrent.duration.*

import core.*
import testkit.*

class LiveSimSpec extends BaseSpec {

  /** A minimal Engine whose `runTo` can be made to sleep, to simulate stalls and slow engines. */
  private class StubEngine extends Engine {
    @volatile private var _tick = 0L

    /** Wall-clock ms of work per sim tick advanced in `runTo` (simulates an engine slower than real time). */
    @volatile var msPerTick: Double = 0.0

    /** One-off stall (ms) applied at the start of the next `runTo` call. */
    @volatile var stallOnceMs: Long = 0L

    def tick: Long = _tick

    def runTo(deadline: Long): Unit = {
      val stall = stallOnceMs
      if (stall > 0) { stallOnceMs = 0; Thread.sleep(stall) }
      val advance = deadline - _tick
      if (advance > 0 && msPerTick > 0) Thread.sleep((advance * msPerTick).toLong)
      if (deadline > _tick) _tick = deadline
    }

    def step(): Unit = ()
    def set(port: Port, value: Option[Boolean]): Unit = ()
    def set(bus: Bus, values: Seq[Boolean]): Unit = ()
    def get(port: Port): Option[Boolean] = None
    def get(bus: Bus): Vector[Option[Boolean]] = bus.map(get).toVector
    def snapshot: EngineState = new EngineState {
      def get(port: Port): Option[Boolean] = None
      def get(bus: Bus): Vector[Option[Boolean]] = bus.map(get).toVector
    }
    def watch(port: Port)(callback: Option[Boolean] => Unit): Unit = ()
  }

  /** Evaluate `f` until it returns a defined value, or fail after `timeoutMs`. */
  private def waitFor[T](timeoutMs: Long = 5000)(f: => Option[T]): T = {
    val deadline = System.currentTimeMillis() + timeoutMs
    var result: Option[T] = f
    while (result.isEmpty && System.currentTimeMillis() < deadline) {
      Thread.sleep(10)
      result = f
    }
    result.getOrElse(throw new AssertionError("timed out waiting for condition"))
  }

  "LiveSim pacing" should {

    "default maxCatchUp to 100 ms" in {
      val sim = new LiveSim(() => new StubEngine)
      try sim.maxCatchUp must beEqualTo(100.millis)
      finally sim.stop()
    }

    "rebase exactly once after a single stall, then pace at the configured rate" in {
      // A controllable clock: the loop still iterates in real time (~5 ms quanta), but all pacing decisions read this
      // clock, so the stall — and the absence of any other stall — is deterministic even on a heavily loaded machine.
      val fakeNanos = new AtomicLong(0L)
      val engine = new StubEngine
      val sim = new LiveSim(() => engine, ticksPerSecond = 1000, nanoTime = () => fakeNanos.get())
      sim.start()
      try {
        Thread.sleep(200) // let the loop spin up; the frozen clock means no rebase is possible
        sim.rebaseCount must beEqualTo(0)

        // a 500 ms stall: the engine sleeps in runTo while 500 ms of (fake) wall time passes
        engine.stallOnceMs = 500
        fakeNanos.addAndGet(500000000L)
        waitFor()(if (sim.rebaseCount == 1) Some(()) else None)

        // subsequent pacing follows the wall clock at ticksPerSecond — the dropped 500 ms is not fast-forwarded
        for (_ <- 1 to 6) {
          val target = engine.tick + 50
          fakeNanos.addAndGet(50000000L)
          waitFor()(if (engine.tick >= target) Some(()) else None)
        }
        engine.tick must beEqualTo(300L)
        sim.rebaseCount must beEqualTo(1)
      } finally sim.stop()
    }

    "report the achieved tick rate" in {
      val fakeNanos = new AtomicLong(0L)
      val engine = new StubEngine
      val sim = new LiveSim(() => engine, ticksPerSecond = 1000, nanoTime = () => fakeNanos.get())
      sim.start()
      try {
        Thread.sleep(200) // let the loop spin up before the clock starts moving
        // pace 2 fake seconds in 50 ms steps; the EWMA converges to the configured rate
        for (_ <- 1 to 40) {
          val target = engine.tick + 50
          fakeNanos.addAndGet(50000000L)
          waitFor()(if (engine.tick >= target) Some(()) else None)
        }
        sim.achievedTicksPerSecond must beBetween(900.0, 1100.0)
      } finally sim.stop()
    }

    "report a bounded lag with repeated rebases when the engine is slower than real time" in {
      val engine = new StubEngine
      // 2 ms of work per tick at 1000 tps: the engine cannot keep up with real time.
      engine.msPerTick = 2.0
      val sim = new LiveSim(() => engine, ticksPerSecond = 1000)
      sim.start()
      try {
        val deadline = System.currentTimeMillis() + 1500
        var maxLag = 0.0
        while (System.currentTimeMillis() < deadline) {
          maxLag = math.max(maxLag, sim.lagMs)
          Thread.sleep(10)
        }

        // repeated rebases: the debt is dropped again and again
        sim.rebaseCount must be_>=(3L)
        // the lag is reported ...
        maxLag must be_>(20.0)
        // ... and bounded by maxCatchUp (100 ms) plus quantum slop, never growing
        maxLag must be_<(250.0)
        // no unbounded catch-up: the engine advanced at its own slow rate, far below 1500 ticks
        engine.tick must be_<(1200L)
        engine.tick must be_>(100L)
      } finally sim.stop()
    }
  }
}
