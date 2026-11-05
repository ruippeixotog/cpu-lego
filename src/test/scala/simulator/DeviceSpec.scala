package simulator

import scala.collection.mutable.ListBuffer

import core.*
import testkit.*

class DeviceSpec extends BaseSpec {

  private def emptyProcessor: GateProcessor =
    GateProcessor.setup(Circuit(Nil, Nil), Config.default, Map.empty)

  private def recordingDevice(
      onAttachBody: DeviceContext => Unit = _ => (),
      onDetachBody: () => Unit = () => ()
  ): Device = new Device {
    def onAttach(ctx: DeviceContext): Unit = onAttachBody(ctx)
    override def onDetach(): Unit = onDetachBody()
  }

  "A device" should {

    "see initial levels at power-on, before the first runTo" in {
      val seen = ListBuffer.empty[Option[Boolean]]
      val dev = recordingDevice(ctx => {
        seen += ctx.read(High)
        seen += ctx.read(Low)
        seen += ctx.read(new Port)
      })
      emptyProcessor.attach(dev)
      seen.toList must beEqualTo(List(Some(true), Some(false), None))
    }

    "run callbacks after the batch, seeing post-batch values only" in {
      val seen = ListBuffer.empty[Option[Boolean]]
      val p = new Port
      val dev = recordingDevice(ctx => ctx.onChange(p)(seen += ctx.read(p)))
      // Both drives land in one batch: mid-batch the port is true, post-batch it is false.
      val sim = emptyProcessor.attach(dev).set(p, true).set(p, false).run()
      seen.toList must beEqualTo(List(Some(false)))
    }

    "fire each registration at most once per batch" in {
      val seen = ListBuffer.empty[String]
      val a, b = new Port
      val dev = recordingDevice(ctx => ctx.onChange(a, b)(seen += "fired"))
      val sim = emptyProcessor.attach(dev).set(a, true).set(b, true).run()
      seen.toList must beEqualTo(List("fired"))
    }

    "run callbacks in attach order, then registration order" in {
      val seen = ListBuffer.empty[String]
      val p = new Port
      val first = recordingDevice(ctx => {
        ctx.onChange(p)(seen += "first-change")
        ctx.onRise(p)(seen += "first-rise")
      })
      val second = recordingDevice(ctx => ctx.onChange(p)(seen += "second-change"))
      val sim = emptyProcessor.attach(first).attach(second).set(p, true).run()
      seen.toList must beEqualTo(List("first-change", "first-rise", "second-change"))
    }

    "fire onRise on None to true and onFall on None to false, but neither on a transition to None" in {
      val seen = ListBuffer.empty[String]
      val p = new Port
      val dev = recordingDevice(ctx => {
        ctx.onRise(p)(seen += "rise")
        ctx.onFall(p)(seen += "fall")
        ctx.onChange(p)(seen += "change")
      })
      val sim = emptyProcessor.attach(dev).set(p, true).run().set(p, None).run().set(p, false).run()
      seen.toList must beEqualTo(List("rise", "change", "change", "fall", "change"))
    }

    "apply a drive exactly delay ticks later" in {
      val in, out = new Port
      val dev = recordingDevice(ctx => ctx.onRise(in)(ctx.drive(out, Some(true), 3)))
      val sim = emptyProcessor.attach(dev).set(in, false).runTo(0).set(in, true).runTo(0)
      sim.runTo(2).get(out) must beNone
      sim.runTo(3).get(out) must beSome(true)
    }

    "let a new drive supersede the same device's pending drives of the pin at or after its time" in {
      val in, out = new Port
      val dev = recordingDevice(ctx => {
        ctx.onRise(in)(ctx.drive(out, Some(true), 5))
        ctx.onFall(in)(ctx.drive(out, Some(false), 2))
      })
      val sim = emptyProcessor.attach(dev).set(in, false).runTo(0).set(in, true).runTo(0).set(in, false).runTo(0)
      sim.runTo(1).get(out) must beNone
      sim.runTo(2).get(out) must beSome(false)
      // The true drive scheduled for tick 5 was cancelled by the false drive scheduled for tick 2.
      sim.runTo(5).get(out) must beSome(false)
    }

    "leave an earlier drive alone when a later drive supersedes only later ones" in {
      val in, out = new Port
      val dev = recordingDevice(ctx => {
        ctx.onRise(in)(ctx.drive(out, Some(true), 2))
        ctx.onFall(in)(ctx.drive(out, Some(false), 5))
      })
      val sim = emptyProcessor.attach(dev).set(in, false).runTo(0).set(in, true).runTo(0).set(in, false).runTo(0)
      sim.runTo(2).get(out) must beSome(true)
      sim.runTo(5).get(out) must beSome(false)
    }

    "release a pin after the release delay" in {
      val in, out = new Port
      val dev = recordingDevice(ctx => {
        ctx.onRise(in)(ctx.drive(out, Some(true), 1))
        ctx.onFall(in)(ctx.release(out, 2))
      })
      val sim = emptyProcessor.attach(dev).set(in, false).runTo(0).set(in, true).runTo(1)
      sim.get(out) must beSome(true)
      val sim2 = sim.set(in, false).runTo(1)
      sim2.runTo(2).get(out) must beSome(true)
      // The release lands at tick 3; the group re-resolves a wire delay later.
      sim2.runTo(4).get(out) must beNone
    }

    "reject a drive delay below 1" in {
      val p = new Port
      val dev = recordingDevice(ctx => ctx.drive(p, Some(true), 0))
      emptyProcessor.attach(dev) must throwA[IllegalArgumentException]
    }

    "report contention between a device and a gate as a fault" in {
      val x, y, gateOut, devicePort, witness = new Port
      val circuit = Circuit(List(NAND(x, y, gateOut)), List((gateOut, devicePort), (gateOut, witness)))
      val dev = recordingDevice(ctx => ctx.drive(devicePort, Some(true), 1))
      val sim = GateProcessor
        .setup(circuit, Config.default, Map.empty)
        .attach(dev)
        .set(x, true)
        .set(y, true)
        .runTo(60)
      sim.get(witness) must beNone
      sim.faults must contain(beLike { case Fault.Contention(_, values) =>
        values.sorted must beEqualTo(List(false, true))
      })
    }

    "read buses bit by bit, marking floating ports" in {
      val seen = ListBuffer.empty[(Long, Long)]
      val bus = Vector(new Port, new Port, new Port)
      val dev = recordingDevice(ctx => seen += ((ctx.readBits(bus), ctx.floatingMask(bus))))
      val sim = emptyProcessor.set(bus(0), true).set(bus(1), false).run().attach(dev)
      seen.toList must beEqualTo(List((1L, 4L)))
    }

    "drive buses with and without a mask" in {
      val in, bus0, bus1, bus2 = new Port
      val bus = Vector(bus0, bus1, bus2)
      val dev = recordingDevice(ctx => {
        ctx.onRise(in)(ctx.driveBits(bus, 7L, 1))
        ctx.onFall(in)(ctx.driveBits(bus, 7L, 5L, 1))
      })
      val sim = emptyProcessor.attach(dev).set(in, false).runTo(0).set(in, true).runTo(1)
      sim.get(bus) must beEqualTo(Vector(Some(true), Some(true), Some(true)))
      val sim2 = sim.set(in, false).runTo(2)
      // The masked release lands at tick 2; the group re-resolves a wire delay later.
      sim2.runTo(3).get(bus) must beEqualTo(Vector(Some(true), None, Some(true)))
    }

    "release driven pins on detach, running onDetach once" in {
      val detached = ListBuffer.empty[String]
      val in, out = new Port
      val dev = recordingDevice(
        ctx => ctx.onRise(in)(ctx.drive(out, Some(true), 1)),
        () => detached += "detached"
      )
      val engine = new GateEngine(emptyProcessor)
      val handle = engine.attach(dev)
      engine.set(in, Some(false))
      engine.runTo(0)
      engine.set(in, Some(true))
      engine.runTo(1)
      engine.get(out) must beSome(true)
      handle.detach()
      engine.runTo(3)
      engine.get(out) must beNone
      handle.detach()
      detached.toList must beEqualTo(List("detached"))
    }

    "leave other devices alone when detaching one" in {
      val in, out = new Port
      val dev = recordingDevice(ctx => ctx.onRise(in)(ctx.drive(out, Some(true), 1)))
      val engine = new GateEngine(emptyProcessor)
      engine.attach(dev)
      val otherHandle = engine.attach(recordingDevice())
      engine.set(in, Some(true))
      engine.runTo(1)
      engine.get(out) must beSome(true)
      otherHandle.detach()
      engine.get(out) must beSome(true)
    }

    "cancel a pending drive on detach" in {
      val in, out = new Port
      val dev = recordingDevice(ctx => ctx.drive(out, Some(true), 5))
      val engine = new GateEngine(emptyProcessor)
      val handle = engine.attach(dev)
      engine.runTo(0)
      handle.detach()
      engine.runTo(10)
      engine.get(out) must beNone
    }

    "attach and detach through the Engine adapter" in {
      val in, out = new Port
      val dev = recordingDevice(ctx => ctx.onRise(in)(ctx.drive(out, Some(true), 1)))
      val engine = new GateEngine(emptyProcessor)
      val handle = engine.attach(dev)
      engine.set(in, Some(true))
      engine.runTo(1)
      engine.get(out) must beSome(true)
      handle.detach()
      engine.set(in, Some(false))
      engine.set(in, Some(true))
      engine.runTo(3)
      engine.get(out) must beNone
    }
  }
}
