package bench

/** Results of benchmarking one workload. */
case class WorkloadResult(
    workload: String,
    description: String,
    iterations: Int,
    totalTicks: Long,
    wallNs: Long,
    transitions: Long // 0 unless --profile
) {

  /** Simulated ticks per wall-clock second. */
  def ticksPerSecond: Double = totalTicks.toDouble / wallNs * 1e9

  /** Simulated megahertz: 1 tick = 1 ns of simulated time. */
  def simulatedMHz: Double = ticksPerSecond / 1e6

  /** Wall-clock nanoseconds per simulated tick. */
  def nsPerTick: Double = wallNs.toDouble / totalTicks

  /** Signal transitions per simulated tick (`--profile` mode only). */
  def transitionsPerTick: Double = transitions.toDouble / totalTicks

  /** Wall-clock nanoseconds per signal transition (`--profile` mode only). */
  def nsPerTransition: Double = wallNs.toDouble / transitions

  /** Wall-clock time spent in device callbacks. Reserved for the device API (#16); zero until devices exist. */
  def deviceCallbackNs: Long = 0L
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
      java.nio.file.Files.writeString(path, Report.markdown(results))
      println(s"wrote $path")
    }
  }

  /** Warm up, then measure iterations until at least `minSeconds` of wall time (bounded), or exactly `fixedIterations`.
    * Returns the aggregated result.
    */
  def run(workload: Workload, opts: Options): WorkloadResult = {
    val run = workload.build()
    (1 to opts.warmup).foreach(_ => run.iterate())

    val countTransitions =
      if (opts.profile) {
        var count = 0L
        run.ports.foreach(port => run.engine.watch(port)(_ => count += 1))
        () => count
      } else () => 0L

    var totalTicks = 0L
    var iters = 0
    val minNs = (opts.minSeconds * 1e9).toLong
    val t0 = System.nanoTime()
    def enough: Boolean = opts.fixedIterations match {
      case Some(n) => iters >= n
      case None => iters >= 3 && (System.nanoTime() - t0 >= minNs || iters >= 100)
    }
    while (!enough) {
      totalTicks += run.iterate()
      iters += 1
    }
    val wallNs = System.nanoTime() - t0
    WorkloadResult(workload.name, workload.description, iters, totalTicks, wallNs, countTransitions())
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
          |                events per tick (adds observer overhead)
          |  --write-results rewrite bench/results.md with this run""".stripMargin)
        sys.exit(0)
      case bad :: _ =>
        println(s"unknown argument '$bad'; see --help")
        sys.exit(1)
      case Nil => o
    }
    loop(args, Options())
  }
}

private[bench] object Report {

  def text(results: List[WorkloadResult], opts: Bench.Options): String = {
    val sb = new StringBuilder
    sb.append("machine:\n")
    MachineInfo.describe.linesIterator.foreach(l => sb.append(s"  $l\n"))
    sb.append(
      f"${"workload"}%-10s ${"iters"}%5s ${"ticks"}%12s ${"wall s"}%8s ${"ticks/s"}%12s ${"MHz"}%8s ${"ns/tick"}%9s"
    )
    if (opts.profile) sb.append(f" ${"ev/tick"}%9s ${"ns/ev"}%9s")
    sb.append("\n")
    results.foreach { r =>
      sb.append(
        f"${r.workload}%-10s ${r.iterations}%5d ${r.totalTicks}%12d ${r.wallNs / 1e9}%8.2f " +
          f"${r.ticksPerSecond}%12.0f ${r.simulatedMHz}%8.4f ${r.nsPerTick}%9.1f"
      )
      if (opts.profile) sb.append(f" ${r.transitionsPerTick}%9.1f ${r.nsPerTransition}%9.1f")
      sb.append("\n")
    }
    sb.toString
  }

  def markdown(results: List[WorkloadResult]): String = {
    val sb = new StringBuilder
    sb.append("# Benchmark results\n\n")
    sb.append(
      "Tracked by hand: optimisation PRs report their gain here. Update with `sbt \"bench --write-results\"`.\n\n"
    )
    sb.append("## Machine\n\n```\n")
    sb.append(MachineInfo.describe)
    sb.append("\n```\n\n## Results\n\n")
    sb.append("| workload | iters | ticks | wall s | ticks/s | MHz | ns/tick |\n")
    sb.append("|---|---|---|---|---|---|---|\n")
    results.foreach { r =>
      sb.append(
        f"| ${r.workload} | ${r.iterations} | ${r.totalTicks} | ${r.wallNs / 1e9}%.2f " +
          f"| ${r.ticksPerSecond}%.0f | ${r.simulatedMHz}%.4f | ${r.nsPerTick}%.1f |\n"
      )
    }
    sb.append("\n_Measured with `sbt bench` (")
    sb.append(java.time.LocalDate.now().toString)
    sb.append(")._\n")
    sb.toString
  }
}
