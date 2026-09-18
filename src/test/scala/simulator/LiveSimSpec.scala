package simulator

import java.util.concurrent.ConcurrentLinkedQueue

import scala.jdk.CollectionConverters.*

import component.BuilderAPI.*
import core.*
import testkit.*

class LiveSimSpec extends BaseSpec {

  /** A minimal hand-written SimEngine, proving LiveSim only depends on the
    * engine interface and not on Sim internals.
    */
  private class StubEngine(
      val tick: Long,
      values: Map[Port, Option[Boolean]],
      observers: Map[Port, List[PortUpdate => Unit]]
  ) extends SimEngine {
    def get(port: Port): Option[Boolean] = values.getOrElse(port, None)
    def set(port: Port, value: Option[Boolean]): SimEngine = {
      val next = new StubEngine(tick, values + (port -> value), observers)
      observers.getOrElse(port, Nil).foreach(_(PortUpdate(port, value, tick)))
      next
    }
    def observe(port: Port, f: PortUpdate => Unit): SimEngine =
      new StubEngine(tick, values, observers + (port -> (f :: observers.getOrElse(port, Nil))))
    def runTo(deadline: Long): SimEngine =
      if (deadline > tick) new StubEngine(deadline, values, observers) else this
    def step(): SimEngine = this
  }

  private def nandCircuit(): (Port, Port, Port, Circuit) = {
    val in1, in2, out = newPort()
    (in1, in2, out, Circuit(List(NAND(in1, in2, out)), Nil))
  }

  private def clockCircuit(freq: Int): (Port, Circuit) = {
    val clk = newPort()
    (clk, Circuit(List(Clock(freq, clk)), Nil))
  }

  "LiveSim" should {

    "drive inputs and read outputs synchronously through the engine interface" in {
      val (in1, in2, out, circuit) = nandCircuit()
      val live = LiveSim(SimEngine.reference(circuit))
      val sub = live.subscribe(out)

      live.get(out) must beNone
      live.set(in1, true)
      live.set(in2, true)
      live.runUntil(100)

      live.get(out) must beSome(false)
      // set@0 -> nand out@1 -> group@2, then idle
      live.tick must beEqualTo(2)
      // exactly one notification despite the redundant observer firing at tick 2
      sub.poll() must beEqualTo(List(PortUpdate(out, Some(false), 1)))
      sub.close()
    }

    "deliver every change once, in tick order" in {
      val (in1, in2, out, circuit) = nandCircuit()
      val live = LiveSim(SimEngine.reference(circuit))
      val sub = live.subscribe(out)

      live.set(in1, true)
      live.set(in2, true)
      live.runUntil(100)
      live.set(in2, false)
      live.runUntil(200)

      val updates = sub.poll()
      updates.map(u => (u.value, u.tick)) must beEqualTo(List((Some(false), 1), (Some(true), 3)))
      sub.close()
    }

    "invoke watch callbacks with tick-stamped updates" in {
      val (in1, in2, out, circuit) = nandCircuit()
      val live = LiveSim(SimEngine.reference(circuit))
      val received = new ConcurrentLinkedQueue[PortUpdate]()
      val closable = live.watch(out)(u => received.offer(u))

      live.set(in1, true)
      live.set(in2, true)
      live.runUntil(100)

      val deadline = System.currentTimeMillis() + 2000
      while (received.isEmpty && System.currentTimeMillis() < deadline) Thread.sleep(10)
      received.asScala.toList must beEqualTo(List(PortUpdate(out, Some(false), 1)))
      closable.close()
    }

    "drive inputs from another thread while running in realtime" in {
      val (in1, in2, out, nandC) = nandCircuit()
      val (clk, clockC) = clockCircuit(5)
      val circuit = Circuit(nandC.components ++ clockC.components, Nil)
      val live = LiveSim(SimEngine.reference(circuit))
      val sub = live.subscribe(out)

      val thread = live.startRealtime(1000) // 1000 ticks/s; clock toggles every 5ms
      try {
        Thread.sleep(50)
        live.set(in1, true) // driven from this thread while the loop runs
        live.set(in2, true)
        Thread.sleep(50)
        val updates = sub.poll()
        updates must not(beEmpty)
        updates.map(_.tick) must beEqualTo(updates.map(_.tick).sorted)
        updates.last.value must beSome(false)
        live.tick must be_>=(50L)
      } finally {
        live.stop()
        thread.join(2000)
        thread.isAlive must beFalse
      }
      sub.close()
    }

    "pace the simulation against the wall clock" in {
      val (_, circuit) = clockCircuit(5)
      val live = LiveSim(SimEngine.reference(circuit))

      val thread = live.startRealtime(1000)
      try {
        Thread.sleep(200)
        // ~200 ticks expected; generous bounds for slow or bursty scheduling
        live.tick must beBetween(50L, 2000L)
      } finally {
        live.stop()
        thread.join(2000)
      }
    }

    "reject a second run loop while one is active" in {
      val (_, circuit) = clockCircuit(5)
      val live = LiveSim(SimEngine.reference(circuit))

      val thread = live.startRealtime(1000)
      try live.runUntil(10) must throwA[IllegalStateException]
      finally {
        live.stop()
        thread.join(2000)
      }
    }

    "work with any SimEngine implementation, not just Sim" in {
      val port = newPort()
      val live = LiveSim(new StubEngine(0, Map(), Map()))
      val sub = live.subscribe(port)

      live.set(port, true)
      live.runUntil(10)

      live.get(port) must beSome(true)
      live.tick must beEqualTo(10)
      sub.poll() must beEqualTo(List(PortUpdate(port, Some(true), 0)))
      sub.close()
    }

    "step one event batch at a time" in {
      val (in1, in2, out, circuit) = nandCircuit()
      val live = LiveSim(SimEngine.reference(circuit))

      live.set(in1, false)
      live.set(in2, true)

      // First step: applies the drives (t=0 batch). NAND output is
      // scheduled for t=1 (gateDelay) but not yet processed.
      live.step()
      live.get(in1) must beSome(false)
      live.get(in2) must beSome(true)
      live.get(out) must beNone

      // Second step: processes the t=1 batch, NAND output becomes true.
      live.step()
      live.get(out) must beSome(true)
    }

    "build directly from a Circuit" in {
      val (in1, in2, out, circuit) = nandCircuit()
      val live = LiveSim(circuit)
      val sub = live.subscribe(out)

      live.set(in1, false)
      live.set(in2, false)
      live.runUntil(10)

      live.get(out) must beSome(true)
      sub.close()
    }

    "not process events beyond the runTo deadline" in {
      val (clk, circuit) = clockCircuit(10)
      val live = LiveSim(SimEngine.reference(circuit))
      val sub = live.subscribe(clk)

      // Clock toggles every 10 ticks. runTo(5) processes the initial
      // setup (t=0) and wire delay (t=1), but must not process the
      // toggle at t=10.
      live.runUntil(5)
      live.tick must beEqualTo(1)
      // No toggle yet: only the initial true at t=0.
      sub.poll().map(_.value) must beEqualTo(List(Some(true)))

      // runTo(15) must process the toggle at t=10 (plus its wire delay at t=11).
      live.runUntil(15)
      live.tick must beEqualTo(11)
      sub.poll().map(_.value) must beEqualTo(List(Some(false)))
      sub.close()
    }
  }
}
