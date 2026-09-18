package simulator

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.LinkedBlockingQueue

import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.*

import component.BuilderAPI.*
import core.*
import testkit.*

class RefSimSpec extends BaseSpec {

  /** A minimal hand-written Sim, proving peripheral code only depends on the Sim interface and not on RefSim.
    */
  private class StubSim extends Sim {
    private val values = TrieMap.empty[Port, Option[Boolean]]
    private val callbacks = TrieMap.empty[Port, List[PortUpdate => Unit]]
    @volatile private var running = false

    def start(): Unit = running = true
    def stop(): Unit = running = false
    def isRunning: Boolean = running

    def set(port: Port, value: Option[Boolean]): Unit = {
      values.put(port, value)
      val u = PortUpdate(port, value)
      callbacks.getOrElse(port, Nil).foreach(_(u))
    }

    def set(bus: Bus, values: Seq[Boolean]): Unit =
      bus.zip(values).foreach { case (p, v) => set(p, Some(v)) }

    def get(port: Port): Option[Boolean] = values.getOrElse(port, None)
    def get(bus: Bus): Vector[Option[Boolean]] = bus.map(get).toVector

    def watch(port: Port)(callback: PortUpdate => Unit): AutoCloseable = {
      callbacks.update(port, callback :: callbacks.getOrElse(port, Nil))
      () => callbacks.update(port, callbacks.getOrElse(port, Nil).filterNot(_ == callback))
    }

    def subscribe(port: Port): PollSubscription = {
      val queue = new LinkedBlockingQueue[PortUpdate]()
      val closable = watch(port) { u =>
        queue.offer(u); ()
      }
      new PollSubscription(port, queue, () => closable.close())
    }
  }

  private def nandCircuit(): (Port, Port, Port, Circuit) = {
    val in1, in2, out = newPort()
    (in1, in2, out, Circuit(List(NAND(in1, in2, out)), Nil))
  }

  private def clockCircuit(freq: Int): (Port, Circuit) = {
    val clk = newPort()
    (clk, Circuit(List(Clock(freq, clk)), Nil))
  }

  /** Evaluate `f` until it returns a defined value, or fail after `timeoutMs`.
    */
  private def waitFor[T](timeoutMs: Long = 5000)(f: => Option[T]): T = {
    val deadline = System.currentTimeMillis() + timeoutMs
    var result: Option[T] = f
    while (result.isEmpty && System.currentTimeMillis() < deadline) {
      Thread.sleep(10)
      result = f
    }
    result.getOrElse(throw new AssertionError("timed out waiting for condition"))
  }

  /** Wait until the subscription has delivered at least one update, and return everything delivered so far.
    */
  private def awaitUpdates(sub: PollSubscription): List[PortUpdate] =
    waitFor() {
      val u = sub.poll()
      if (u.nonEmpty) Some(u) else None
    }

  "RefSim" should {

    "start and stop" in {
      val (_, circuit) = clockCircuit(10)
      val sim = RefSim(circuit)

      sim.isRunning must beFalse
      sim.start()
      sim.isRunning must beTrue
      sim.stop()
      sim.isRunning must beFalse
      // stopping an idle sim is safe, and it can be restarted
      sim.stop()
      sim.start()
      sim.isRunning must beTrue
      sim.stop()
      sim.isRunning must beFalse
    }

    "reject a second start while running" in {
      val (_, circuit) = clockCircuit(10)
      val sim = RefSim(circuit)

      sim.start()
      try sim.start() must throwA[IllegalStateException]
      finally sim.stop()
    }

    "drive inputs and observe outputs while running" in {
      val (in1, in2, out, circuit) = nandCircuit()
      val sim = RefSim(circuit)
      val sub = sim.subscribe(out)

      sim.get(out) must beNone
      sim.start()
      try {
        sim.set(in1, true)
        sim.set(in2, true)

        awaitUpdates(sub) must beEqualTo(List(PortUpdate(out, Some(false))))
        sim.get(out) must beSome(false)
      } finally {
        sim.stop()
        sub.close()
      }
    }

    "drive a whole bus at once" in {
      val (in1, in2, out, circuit) = nandCircuit()
      val sim = RefSim(circuit)
      val sub = sim.subscribe(out)

      sim.start()
      try {
        sim.set(Vector(in1, in2), Seq(true, true))

        awaitUpdates(sub) must beEqualTo(List(PortUpdate(out, Some(false))))
      } finally {
        sim.stop()
        sub.close()
      }
    }

    "deliver every change once, in simulation order" in {
      val (in1, in2, out, circuit) = nandCircuit()
      val sim = RefSim(circuit)
      val sub = sim.subscribe(out)

      sim.start()
      try {
        sim.set(in1, true)
        sim.set(in2, true)
        awaitUpdates(sub) must beEqualTo(List(PortUpdate(out, Some(false))))

        sim.set(in2, false)
        awaitUpdates(sub) must beEqualTo(List(PortUpdate(out, Some(true))))
      } finally {
        sim.stop()
        sub.close()
      }
    }

    "invoke watch callbacks off the simulation thread" in {
      val (in1, in2, out, circuit) = nandCircuit()
      val sim = RefSim(circuit)
      val received = new ConcurrentLinkedQueue[(String, PortUpdate)]()
      val closable = sim.watch(out) { u =>
        received.add((Thread.currentThread().getName, u)); ()
      }

      sim.start()
      try {
        sim.set(in1, true)
        sim.set(in2, true)

        val updates = waitFor() {
          val u = received.asScala.toList
          if (u.nonEmpty) Some(u) else None
        }
        updates.map(_._2) must beEqualTo(List(PortUpdate(out, Some(false))))
        updates.map(_._1).distinct must beEqualTo(List("refsim-notifier"))
      } finally {
        sim.stop()
        closable.close()
      }
    }

    "isolate throwing watch callbacks" in {
      val (in1, in2, out, circuit) = nandCircuit()
      val sim = RefSim(circuit)
      val received = new ConcurrentLinkedQueue[PortUpdate]()
      sim.watch(out)(_ => throw new RuntimeException("boom"))
      sim.watch(out) { u =>
        received.add(u); ()
      }

      sim.start()
      try {
        sim.set(in1, true)
        sim.set(in2, true)

        val updates = waitFor() {
          val u = received.asScala.toList
          if (u.nonEmpty) Some(u) else None
        }
        updates must beEqualTo(List(PortUpdate(out, Some(false))))
        // the loop survived the throwing callback
        sim.isRunning must beTrue
      } finally sim.stop()
    }

    "drive inputs from watch callbacks" in {
      val (in1, in2, out, circuit) = nandCircuit()
      val sim = RefSim(circuit)
      val sub = sim.subscribe(out)
      // when in1 goes high, drive in2 high as well
      sim.watch(in1) { u => if (u.value.contains(true)) sim.set(in2, true) }

      sim.start()
      try {
        sim.set(in1, true)

        awaitUpdates(sub) must beEqualTo(List(PortUpdate(out, Some(false))))
      } finally {
        sim.stop()
        sub.close()
      }
    }

    "stop delivering after unsubscribe" in {
      val (in1, in2, out, circuit) = nandCircuit()
      val sim = RefSim(circuit)
      val sub = sim.subscribe(out)

      sim.start()
      try {
        sim.set(in1, true)
        sim.set(in2, true)
        awaitUpdates(sub)
        sub.close()

        sim.set(in2, false) // would flip the output
        Thread.sleep(200)
        sub.poll() must beEmpty
      } finally sim.stop()
    }

    "pace the simulation against the wall clock" in {
      val (clk, circuit) = clockCircuit(10) // toggles every 10 ticks
      val sim = RefSim(circuit, ticksPerSecond = 1000)
      val sub = sim.subscribe(clk)

      sim.start()
      Thread.sleep(200) // ~200 ticks -> ~20 toggles
      sim.stop()
      Thread.sleep(100) // let the notifier drain

      val toggles = sub.poll()
      // generous bounds for slow or bursty scheduling
      toggles.size must beBetween(5, 60)
      // values strictly alternate, starting high
      toggles.map(_.value) must beEqualTo(
        toggles.indices.map(i => Some(i % 2 == 0)).toList
      )
      sub.close()
    }

    "work with any Sim implementation, not just RefSim" in {
      val in, out = newPort()
      val sim: Sim = new StubSim

      // a tiny peripheral — an inverter written only against the Sim trait
      sim.watch(in) { u => sim.set(out, u.value.map(!_)) }

      sim.isRunning must beFalse
      sim.start()
      sim.isRunning must beTrue

      sim.set(in, true)
      sim.get(out) must beSome(false)
      sim.set(in, false)
      sim.get(out) must beSome(true)

      sim.stop()
      sim.isRunning must beFalse
    }
  }
}
