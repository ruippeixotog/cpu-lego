package simulator

import core.*

/** An [[Engine]] adapter over the functional [[GateProcessor]].
  *
  * The gate-level simulation itself stays purely functional: every `GateProcessor` operation returns a new processor,
  * and this adapter just swaps the current one in.
  */
final class GateEngine(private var p: GateProcessor) extends Engine {

  def set(port: Port, value: Option[Boolean]): Unit = p = p.set(port, value)

  def set(bus: Bus, values: Seq[Boolean]): Unit = p = p.set(bus, values)

  def get(port: Port): Option[Boolean] = p.get(port)

  def get(bus: Bus): Vector[Option[Boolean]] = p.get(bus)

  def runTo(deadline: Long): Unit = p = p.runTo(deadline)

  def step(): Unit = p = p.step()

  def tick: Long = p.tick

  def snapshot: EngineState = p

  def watch(port: Port)(callback: Option[Boolean] => Unit): Unit =
    p = p.watch(port) { q =>
      callback(q.get(port)); q
    }
}
