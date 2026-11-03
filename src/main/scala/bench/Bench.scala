package bench

import simulator.{GateEngine, GateProcessor}

/** Results of benchmarking one workload. */
case class WorkloadResult(
    workload: String,
    iterations: Int,
    totalEdges: Long,
    wallNs: Long,
    transitions: Long // 0 unless --profile
) {

  /** Clock edges per wall-clock second. */
  def edgesPerSecond: Double = totalEdges.toDouble / wallNs * 1e9

  /** Wall-clock nanoseconds per clock edge. */
  def nsPerEdge: Double = wallNs.toDouble / totalEdges

  /** Signal transitions per clock edge (`--profile` mode only). */
  def transitionsPerEdge: Double = transitions.toDouble / totalEdges

  /** Wall-clock nanoseconds per signal transition (`--profile` mode only). */
  def nsPerTransition: Double = wallNs.toDouble / transitions
}

/** Best-effort description of the machine a benchmark ran on, for `bench/results.md`. */
object MachineInfo {

  def describe: String = {
    val cpu = readFirst("/proc/cpuinfo", "model name").getOrElse("unknown")
    val memKb = readFirst("/proc/meminfo", "MemTotal").getOrElse("unknown")
    s"""OS: ${sys.props.getOrElse("os.name", "?")} ${sys.props.getOrElse("os.arch", "?")}
       |CPU: $cpu x${Runtime.getRuntime.availableProcessors}
       |Memory: $memKb
       |JVM: ${sys.props.getOrElse("java.version", "?")} (${sys.props.getOrElse("java.vendor", "?")})""".stripMargin
  }

  private def readFirst(file: String, prefix: String): Option[String] =
    try {
      val src = scala.io.Source.fromFile(file)
      try src.getLines().find(_.startsWith(prefix)).map(_.split(":", 2)(1).trim)
      finally src.close()
    } catch { case _: Exception => None }
}

object Bench {

  /** The gate-level engine, Bench's default choice. */
  val gateEngineFactory: Workload.EngineFactory =
    (circuit, conf, portNames) => GateEngine(GateProcessor.setup(circuit, conf, portNames))

  case class Options(
      workload: Option[String] = None,
      warmup: Int = 2,
      fixedIterations: Option[Int] = None,
      minSeconds: Double = 2.0,
      profile: Boolean = false,
      writeResults: Boolean = false
  )

  def main(args: Array[String]): Unit = {
    val opts = parse(args.toList)
    val selected = opts.workload match {
      case Some(name) =>
        Workloads.named(name) match {
          case Some(w) => List(w)
          case None =>
            println(s"unknown workload '$name'; available: ${Workloads.all.map(_.name).mkString(", ")}")
            sys.exit(1)
        }
      case None => Workloads.all
    }
    val results = selected.map(run(_, opts))
    println(Report.text(results, opts))
    if (opts.writeResults) {
      val path = java.nio.file.Paths.get("bench/results.md")
      val previous =
        if (java.nio.file.Files.exists(path)) Some(java.nio.file.Files.readString(path)) else None
      java.nio.file.Files.writeString(path, Report.markdown(results, previous))
      println(s"wrote $path")
    }
  }

  /** Warm up, then measure iterations until at least `minSeconds` of wall time (bounded), or exactly `fixedIterations`.
    * Returns the aggregated result.
    */
  def run(
      workload: Workload,
      opts: Options,
      engineFactory: Workload.EngineFactory = gateEngineFactory
  ): WorkloadResult = {
    val run = workload.build(engineFactory)
    (1 to opts.warmup).foreach(_ => run.iterate())

    val countTransitions =
      if (opts.profile) {
        var count = 0L
        run.ports.foreach(port => run.engine.watch(port)(_ => count += 1))
        () => count
      } else () => 0L

    var totalEdges = 0L
    var iters = 0
    val minNs = (opts.minSeconds * 1e9).toLong
    val t0 = System.nanoTime()
    def enough: Boolean = opts.fixedIterations match {
      case Some(n) => iters >= n
      case None => iters >= 3 && (System.nanoTime() - t0 >= minNs || iters >= 100)
    }
    while (!enough) {
      totalEdges += run.iterate()
      iters += 1
    }
    val wallNs = System.nanoTime() - t0
    WorkloadResult(workload.name, iters, totalEdges, wallNs, countTransitions())
  }

  private def parse(args: List[String]): Options = {
    def loop(rest: List[String], o: Options): Options = rest match {
      case "--workload" :: name :: tail => loop(tail, o.copy(workload = Some(name)))
      case "--warmup" :: n :: tail => loop(tail, o.copy(warmup = n.toInt))
      case "--iterations" :: n :: tail => loop(tail, o.copy(fixedIterations = Some(n.toInt)))
      case "--min-seconds" :: s :: tail => loop(tail, o.copy(minSeconds = s.toDouble))
      case "--profile" :: tail => loop(tail, o.copy(profile = true))
      case "--write-results" :: tail => loop(tail, o.copy(writeResults = true))
      case "--help" :: _ =>
        println("""usage: sbt "bench [--workload <name>] [--warmup <n>] [--iterations <n>]
          |                [--min-seconds <s>] [--profile] [--write-results]"
          |
          |  --workload      only run this workload (default: all registered)
          |  --warmup        unmeasured warmup iterations (default 2)
          |  --iterations    fixed measured iteration count (default: run until
          |                --min-seconds of wall time, at least 3 iterations)
          |  --min-seconds   minimum measured wall time per workload (default 2.0)
          |  --profile       count signal transitions via engine watch() and report
          |                events per edge (adds observer overhead)
          |  --write-results merge this run's rows into bench/results.md""".stripMargin)
        sys.exit(0)
      case bad :: _ =>
        println(s"unknown argument '$bad'; see --help")
        sys.exit(1)
      case Nil => o
    }
    val opts = loop(args, Options())
    if (opts.profile && opts.writeResults) {
      println("--profile cannot be combined with --write-results: profiled numbers must not become the baseline")
      sys.exit(1)
    }
    opts
  }
}

private[bench] object Report {

  def text(results: List[WorkloadResult], opts: Bench.Options): String = {
    val sb = new StringBuilder
    sb.append("machine:\n")
    MachineInfo.describe.linesIterator.foreach(l => sb.append(s"  $l\n"))
    sb.append(
      f"${"workload"}%-10s ${"iters"}%5s ${"edges"}%12s ${"wall s"}%8s ${"edges/s"}%12s ${"ns/edge"}%9s"
    )
    if (opts.profile) sb.append(f" ${"ev/edge"}%9s ${"ns/ev"}%9s")
    sb.append("\n")
    results.foreach { r =>
      sb.append(
        f"${r.workload}%-10s ${r.iterations}%5d ${r.totalEdges}%12d ${r.wallNs / 1e9}%8.2f " +
          f"${r.edgesPerSecond}%12.0f ${r.nsPerEdge}%9.1f"
      )
      if (opts.profile) sb.append(f" ${r.transitionsPerEdge}%9.1f ${r.nsPerTransition}%9.1f")
      sb.append("\n")
    }
    sb.toString
  }

  /** Render the results table, merging this run's rows into a previous table by workload name: rerun workloads are
    * replaced in place, untouched rows are kept, new workloads are appended. Rows in an older column layout are
    * dropped, since their columns no longer exist.
    */
  def markdown(results: List[WorkloadResult], previous: Option[String] = None): String = {
    val sb = new StringBuilder
    sb.append("# Benchmark results\n\n")
    sb.append("Optimisation PRs report their gain here. Update with `sbt \"bench --write-results\"`.\n\n")
    sb.append("## Machine\n\n```\n")
    sb.append(MachineInfo.describe)
    sb.append("\n```\n\n## Results\n\n")
    sb.append("| workload | iters | edges | wall s | edges/s | ns/edge |\n")
    sb.append("|---|---|---|---|---|---|\n")
    val freshRows = results.map(r => r.workload -> row(r)).toMap
    val previousRows = previous.toList.flatMap(parseRows)
    previousRows.foreach { case (name, oldRow) => sb.append(freshRows.getOrElse(name, oldRow)).append("\n") }
    results.filterNot(r => previousRows.exists(_._1 == r.workload)).foreach(r => sb.append(row(r)).append("\n"))
    sb.append("\n_Measured with `sbt bench` (")
    sb.append(java.time.LocalDate.now().toString)
    sb.append(")._\n")
    sb.toString
  }

  private def row(r: WorkloadResult): String =
    f"| ${r.workload} | ${r.iterations} | ${r.totalEdges} | ${r.wallNs / 1e9}%.2f " +
      f"| ${r.edgesPerSecond}%.0f | ${r.nsPerEdge}%.1f |"

  /** Rows of a previous results table, as (workload, row) pairs in table order. */
  private def parseRows(text: String): List[(String, String)] =
    text.linesIterator.toList.flatMap { line =>
      val cells = line.split("\\|", -1).map(_.trim).toList
      // "| workload | iters | edges | wall s | edges/s | ns/edge |" splits into 8 cells (empty ends)
      if (cells.length == 8 && cells(2).forall(_.isDigit) && cells(2).nonEmpty) Some((cells(1), line)) else None
    }
}
