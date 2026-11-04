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

/** Best-effort description of the machine a benchmark ran on, for the console report and `bench/results.md`. */
object MachineInfo {

  /** CPU model with processor count, e.g. `AMD EPYC 9D25 126-Core Processor x4`. */
  def cpu: String = {
    val model = readFirst("/proc/cpuinfo", "model name")
      .orElse(sysctl("machdep.cpu.brand_string"))
      .getOrElse("unknown")
    s"$model x${Runtime.getRuntime.availableProcessors}"
  }

  /** JVM version and vendor, e.g. `25.0.4.1 (Eclipse Adoptium)`. */
  def jvm: String =
    s"${sys.props.getOrElse("java.version", "?")} (${sys.props.getOrElse("java.vendor", "?")})"

  def describe: String = {
    val memKb = readFirst("/proc/meminfo", "MemTotal").getOrElse("unknown")
    s"""OS: ${sys.props.getOrElse("os.name", "?")} ${sys.props.getOrElse("os.arch", "?")}
       |CPU: $cpu
       |Memory: $memKb
       |JVM: $jvm""".stripMargin
  }

  private def readFirst(file: String, prefix: String): Option[String] =
    try {
      val src = scala.io.Source.fromFile(file)
      try src.getLines().find(_.startsWith(prefix)).map(_.split(":", 2)(1).trim)
      finally src.close()
    } catch { case _: Exception => None }

  private def sysctl(key: String): Option[String] =
    try {
      val out = new StringBuilder
      val code = scala.sys.process
        .Process(Seq("sysctl", "-n", key))
        .!(scala.sys.process.ProcessLogger(s => { out.append(s); () }))
      if (code == 0) Some(out.toString.trim).filter(_.nonEmpty) else None
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
    if (opts.writeResults) ResultsLog.write(results) match {
      case Left(err) =>
        println(s"refusing to write bench/results.md: $err")
        sys.exit(1)
      case Right(path) => println(s"appended ${results.size} rows to $path")
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
          |  --write-results append the full suite's rows to bench/results.md
          |                (only on master with a clean tree; cannot be combined
          |                with --workload or --profile)""".stripMargin)
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
    if (opts.workload.isDefined && opts.writeResults) {
      println(
        "--workload cannot be combined with --write-results: the file must record one full-suite run on one machine"
      )
      sys.exit(1)
    }
    opts
  }
}

/** The log of `master`'s performance in `bench/results.md`: one appended row per workload per run.
  *
  * Only the repo owner writes it, from their machine, on `master` — `write` refuses unless HEAD is `master` and the
  * working tree is clean apart from `bench/results.md` itself, so every row maps to a commit anyone can check out.
  * Optimisation PRs never touch the file; they report base vs head in the PR description instead.
  */
private[bench] object ResultsLog {
  val relativePath = "bench/results.md"

  /** Check the write preconditions; on success the short commit the new rows will cite. */
  def guard(cwd: java.io.File = new java.io.File(".")): Either[String, String] = {
    def git(args: String*): Either[String, String] =
      try {
        val out = new StringBuilder
        val err = new StringBuilder
        val code = scala.sys.process
          .Process(Seq("git") ++ args, cwd)
          .!(
            scala.sys.process
              .ProcessLogger(s => { out.append(s).append("\n"); () }, s => { err.append(s).append("\n"); () })
          )
        if (code == 0) Right(out.toString.trim) else Left(err.toString.trim)
      } catch { case e: Exception => Left(e.getMessage) }
    for {
      branch <- git("rev-parse", "--abbrev-ref", "HEAD").left.map(_ => "cannot determine the current branch")
      _ <- Either.cond(branch == "master", (), s"HEAD is on '$branch', not 'master'")
      status <- git("status", "--porcelain").left.map(_ => "cannot read the working tree status")
      stray = status.linesIterator
        .filter(_.nonEmpty)
        .map { line =>
          val path = line.drop(3)
          if (path.contains(" -> ")) path.split(" -> ", 2)(1) else path
        }
        .filterNot(_ == relativePath)
        .toList
      _ <- Either.cond(stray.isEmpty, (), s"working tree has changes outside $relativePath: ${stray.mkString(", ")}")
      commit <- git("rev-parse", "--short", "HEAD").left.map(_ => "cannot determine HEAD's commit")
    } yield commit
  }

  /** Append one row per result to the log file, creating it from the header if absent. */
  def write(results: List[WorkloadResult]): Either[String, java.nio.file.Path] =
    guard() match {
      case Left(err) => Left(err)
      case Right(commit) =>
        val path = java.nio.file.Paths.get(relativePath)
        val existing =
          if (java.nio.file.Files.exists(path)) java.nio.file.Files.readString(path) else ""
        java.nio.file.Files.writeString(
          path,
          Report.append(existing, results, java.time.LocalDate.now().toString, commit)
        )
        Right(path)
    }
}

private[bench] object Report {

  /** The log file's header: title, policy, and the table header with no rows. */
  def header: String =
    """# Benchmark results
      |
      |Log of `master`'s performance, updated only by the repo owner from their machine on `master`
      |with `sbt "bench --write-results"`. Optimisation PRs never touch this file; they report base
      |vs head, measured back-to-back on the same machine, in the PR description instead.
      |
      || date | commit | workload | iters | edges/s | ns/edge | JVM | CPU |
      ||---|---|---|---|---|---|---|---|
      |""".stripMargin

  /** One log row per result, each carrying its own date, commit, JVM and CPU. */
  def rows(results: List[WorkloadResult], date: String, commit: String): String =
    results
      .map { r =>
        f"| $date | $commit | ${r.workload} | ${r.iterations} | ${r.edgesPerSecond}%.0f | ${r.nsPerEdge}%.1f | ${MachineInfo.jvm} | ${MachineInfo.cpu} |"
      }
      .mkString("", "\n", "\n")

  /** Append a run's rows to existing log content (the header when the file is new). */
  def append(existing: String, results: List[WorkloadResult], date: String, commit: String): String = {
    val base = if (existing.isEmpty) header else if (existing.endsWith("\n")) existing else existing + "\n"
    base + rows(results, date, commit)
  }

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

}
