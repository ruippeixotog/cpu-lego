package bench

import core.*
import simulator.{Circuit, Config, Engine}

/** A benchmark workload: a circuit plus an engine-native driver that exercises it.
  *
  * Workloads are black-box with respect to the engine: the driver only uses the [[Engine]] interface
  * (set/get/runTo/step/watch/tick), never simulator internals, so the same workload measures any engine Bench builds
  * for it.
  */
trait Workload {

  /** Short unique id, used for `--workload` selection. */
  def name: String

  /** Build a fresh workload run: a new engine and driver state. Called once per benchmark, not per iteration.
    * `engineFactory` is Bench's choice of engine, built from the workload's circuit, config and port names.
    */
  def build(engineFactory: Workload.EngineFactory): WorkloadRun
}

object Workload {

  /** Builds the engine a workload run measures: Bench chooses the factory, the workload supplies its circuit, config
    * and hierarchical port names (for fault reports).
    */
  type EngineFactory = (Circuit, Config, Map[Port, String]) => Engine
}

/** One benchmarkable run of a [[Workload]]: holds the engine and knows how to drive one iteration. */
trait WorkloadRun {

  /** The engine under measurement. */
  def engine: Engine

  /** The circuit under measurement. */
  def circuit: Circuit

  /** Drive exactly one benchmark iteration (e.g. run the demo program to completion). Returns the number of clock
    * edges driven during the iteration. Must be deterministic: repeated calls return the same count.
    */
  def iterate(): Long

  /** Every port on the circuit's wires, deduplicated, for profiling transition counts. The harness installs `watch`
    * callbacks on these in `--profile` mode.
    */
  def ports: Seq[Port] =
    circuit.wires.flatMap { case (a, b) => Seq(a, b) }.distinct
}

/** The workload registry: every benchmark in the suite. To add a workload, implement [[Workload]] and append it here;
  * see `bench/README.md`.
  */
object Workloads {

  val all: List[Workload] = List(
    Sap1Workload
  )

  def named(name: String): Option[Workload] = all.find(_.name == name)
}
