package bench

import core.*
import simulator.Engine

/** A benchmark workload: a circuit plus an engine-native driver that exercises it.
  *
  * Workloads are black-box with respect to the engine: the driver only uses the [[Engine]] interface
  * (set/get/runTo/step/watch/tick), never simulator internals, so the same workload measures any engine faithfully.
  */
trait Workload {

  /** Short unique id, used for `--workload` selection. */
  def name: String

  /** One-line description for the benchmark report. */
  def description: String

  /** Build a fresh workload run: a new engine and driver state. Called once per benchmark, not per iteration. */
  def build(): WorkloadRun
}

/** One benchmarkable run of a [[Workload]]: holds the engine and knows how to drive one iteration. */
trait WorkloadRun {

  /** The engine under measurement. */
  def engine: Engine

  /** Drive exactly one benchmark iteration (e.g. run the demo program to completion). Returns the number of simulation
    * ticks advanced. Must be deterministic: repeated calls advance by the same tick count.
    */
  def iterate(): Long

  /** Every port of the circuit, for profiling transition counts. The harness installs `watch` callbacks on these in
    * `--profile` mode.
    */
  def ports: Seq[Port]
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
