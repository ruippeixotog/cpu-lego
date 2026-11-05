package simulator

import scala.compiletime.uninitialized

import core.*

/** A peripheral that lives in simulated time, like a chip on the board.
  *
  * A device reacts to pin changes at the simulated instant they happen and drives pins after a propagation delay, so it
  * can answer a CPU bus cycle within a bounded number of simulated ticks. That is the whole point: an asynchronous
  * watcher runs in wall-clock time and cannot answer in bounded simulated time, while a device is part of the
  * simulation itself.
  *
  * The hardware rule shapes the API. A device sees pins only: it reads levels, reacts to edges, and drives outputs with
  * a delay, exactly what probing package pins and propagation delay give a real chip. It never sees ticks, stepping, or
  * pacing — a device that needs time counts edges of a clock pin wired to it, or uses its own delayed drives as a
  * timer, the way a monostable or oscillator chip would. Ticks, stepping and pacing are never exposed.
  *
  * Determinism is a requirement, not an aspiration: a device's callbacks must be deterministic functions of their
  * callback history plus explicitly queued host input, so golden runs and parity between engines hold. Host threads
  * (UI, audio, files) are reached only through non-blocking structures, never by blocking, and a device never starts or
  * stops the simulation. Callbacks run on the thread driving the engine, synchronously, at the simulated instant; they
  * must return quickly and must not drive the engine except through the [[DeviceContext]] they were given.
  *
  * Attach and detach are engine operations, not context operations: they take effect at the current simulated instant,
  * outside any callback batch. A device attached before the first `runTo` gets `onAttach` at tick 0 and sees the
  * initial levels. Detach cancels the device's pending drives, releases every pin it drives, removes its sensitivities,
  * and then runs `onDetach`.
  */
trait Device {

  /** Register sensitivities on `ctx` and read the current pin levels. Runs once, at attach, at the current simulated
    * instant. No synthetic edges fire for pins that are already high.
    */
  def onAttach(ctx: DeviceContext): Unit

  /** Runs once, at detach, after the device's drives are cancelled, its pins released, and its sensitivities removed.
    */
  def onDetach(): Unit = ()
}

/** What a [[Device]] may do, and only what it may do.
  *
  * The context is valid only while `onAttach` or a callback runs: reads return values at the current simulated instant,
  * and drives schedule into the simulation from that instant. Holding a context past the call that received it and
  * using it later reads stale state.
  *
  * Callback timing: a callback for a change at tick `t` runs after all events of tick `t` are processed, at the end of
  * the batch — never mid-batch — and sees the post-batch values. A registration fires at most once per batch: a port
  * that changes twice within one batch fires for its net change only. Multiple callbacks at the same tick run in a
  * deterministic order: device attach order, then registration order.
  *
  * Edges: `onRise` fires on `Some(false)` or `None` to `Some(true)`; `onFall` fires on `Some(true)` or `None` to
  * `Some(false)`. A transition to `None` fires neither edge — only `onChange` sees it — and a transition from `None`
  * counts as an edge into the new level.
  *
  * Drives: a device acts as a driver in the wire group, like a gate output, so multiple disagreeing drivers are
  * contention. `delay` is in ticks and must be at least 1: a drive lands exactly `delay` ticks after the instant it was
  * issued, never sooner. Drives are inertial per device and pin: a new drive of a pin by a device cancels that device's
  * pending drives of the same pin scheduled at or after the new drive's time. A drive of `None` releases the pin.
  *
  * Buses: bit `i` of a value is bus port `i`. [[readBits]] counts a floating port as 0; [[floatingMask]] marks floating
  * ports with 1 bits. [[driveBits]] with a mask drives the masked bits to the value's bits and releases the rest.
  */
trait DeviceContext {

  /** Run `f` when `port` rises. */
  def onRise(port: Port)(f: => Unit): Unit

  /** Run `f` when `port` falls. */
  def onFall(port: Port)(f: => Unit): Unit

  /** Run `f` once per batch in which any of `ports` changes. */
  def onChange(ports: Port*)(f: => Unit): Unit

  /** The port's effective value at the current simulated instant. */
  def read(port: Port): Option[Boolean]

  /** The bus value at the current simulated instant, bit `i` from bus port `i`; floating ports count as 0. */
  def readBits(bus: Bus): Long

  /** A bitmask with 1 bits for the bus ports that are floating at the current simulated instant. */
  def floatingMask(bus: Bus): Long

  /** Drive the port to `value` after `delay` ticks; `None` releases it. */
  def drive(port: Port, value: Option[Boolean], delay: Int = 1): Unit

  /** Drive every port of the bus to the matching bit of `bits` after `delay` ticks. */
  def driveBits(bus: Bus, bits: Long, delay: Int = 1): Unit

  /** Drive the masked bits of the bus to the matching bits of `bits` after `delay` ticks, and release the rest. */
  def driveBits(bus: Bus, bits: Long, mask: Long, delay: Int): Unit

  /** Release the port after `delay` ticks. */
  def release(port: Port, delay: Int = 1): Unit

  /** Release every port of the bus after `delay` ticks. */
  def release(bus: Bus, delay: Int): Unit
}

/** The result of attaching a device: detaching through it removes the device from the engine. Detach is idempotent.
  */
trait DeviceHandle {

  /** Detach the device: cancel its pending drives, release its pins, remove its sensitivities, run `onDetach`. */
  def detach(): Unit
}

/** A sensitivity registration: when it fires for a batch's changes, and what to run. */
private[simulator] sealed trait DeviceRegistration {
  def fires(candidates: Set[Port], before: GateProcessor, after: GateProcessor): Boolean
  def run(): Unit
}

private[simulator] final case class OnRiseRegistration(port: Port, f: () => Unit) extends DeviceRegistration {
  def fires(candidates: Set[Port], before: GateProcessor, after: GateProcessor): Boolean =
    candidates.contains(port) && before.get(port) != Some(true) && after.get(port) == Some(true)
  def run(): Unit = f()
}

private[simulator] final case class OnFallRegistration(port: Port, f: () => Unit) extends DeviceRegistration {
  def fires(candidates: Set[Port], before: GateProcessor, after: GateProcessor): Boolean =
    candidates.contains(port) && before.get(port) != Some(false) && after.get(port) == Some(false)
  def run(): Unit = f()
}

private[simulator] final case class OnChangeRegistration(ports: Vector[Port], f: () => Unit)
    extends DeviceRegistration {
  def fires(candidates: Set[Port], before: GateProcessor, after: GateProcessor): Boolean =
    ports.exists(p => candidates.contains(p) && before.get(p) != after.get(p))
  def run(): Unit = f()
}

/** One attached device: the device itself, the context its callbacks close over, and the pins it currently drives with
  * a non-floating value, for detach.
  */
private[simulator] final case class DeviceEntry(
    device: Device,
    ctx: DeviceContextImpl,
    driven: Map[Port, Boolean]
)

/** The [[DeviceContext]] implementation: a thin facade over the processor the engine is currently threading through a
  * batch. The engine sets [[current]] before invoking `onAttach` or a callback and reads it back after, so drives
  * issued through the context land in the threaded processor. Registrations accumulate in call order.
  */
private[simulator] final class DeviceContextImpl(val device: Device) extends DeviceContext {
  private[simulator] var current: GateProcessor = uninitialized
  private[simulator] var registrations: Vector[DeviceRegistration] = Vector.empty

  def onRise(port: Port)(f: => Unit): Unit =
    registrations :+= OnRiseRegistration(port, () => f)

  def onFall(port: Port)(f: => Unit): Unit =
    registrations :+= OnFallRegistration(port, () => f)

  def onChange(ports: Port*)(f: => Unit): Unit =
    registrations :+= OnChangeRegistration(ports.toVector, () => f)

  def read(port: Port): Option[Boolean] = current.get(port)

  def readBits(bus: Bus): Long = {
    require(bus.length <= 64, "readBits supports buses up to 64 ports")
    bus.zipWithIndex.foldLeft(0L) { case (acc, (p, i)) =>
      if (current.get(p) == Some(true)) acc | (1L << i) else acc
    }
  }

  def floatingMask(bus: Bus): Long = {
    require(bus.length <= 64, "floatingMask supports buses up to 64 ports")
    bus.zipWithIndex.foldLeft(0L) { case (acc, (p, i)) =>
      if (current.get(p).isEmpty) acc | (1L << i) else acc
    }
  }

  def drive(port: Port, value: Option[Boolean], delay: Int = 1): Unit = {
    require(delay >= 1, "delay must be >= 1")
    current = current.driveDevice(device, port, value, delay)
  }

  def driveBits(bus: Bus, bits: Long, delay: Int = 1): Unit = {
    require(delay >= 1, "delay must be >= 1")
    require(bus.length <= 64, "driveBits supports buses up to 64 ports")
    bus.zipWithIndex.foreach { case (p, i) => drive(p, Some(((bits >> i) & 1L) == 1L), delay) }
  }

  def driveBits(bus: Bus, bits: Long, mask: Long, delay: Int): Unit = {
    require(delay >= 1, "delay must be >= 1")
    require(bus.length <= 64, "driveBits supports buses up to 64 ports")
    bus.zipWithIndex.foreach { case (p, i) =>
      if (((mask >> i) & 1L) == 1L) drive(p, Some(((bits >> i) & 1L) == 1L), delay)
      else drive(p, None, delay)
    }
  }

  def release(port: Port, delay: Int = 1): Unit = drive(port, None, delay)

  def release(bus: Bus, delay: Int): Unit = driveBits(bus, 0L, 0L, delay)
}
