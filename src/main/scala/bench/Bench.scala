package bench

import simulator.{GateEngine, GateProcessor}

/** One fork's measurement of a workload: the median ns/edge across its measured iterations. */
case class ForkMeasurement(
    medianNsPerEdge: Double,
    iterations: Int,
    totalEdges: Long,
    transitionsPerEdge: Double // 0 unless --profile
)

/** A workload's result across all forks: the median, min and max of the forks' median ns/edge.
  *
  * Per-JVM offsets dominate the noise (the issue's findings: separate JVMs on one commit differ by 5–14% while
  * iterations inside one JVM vary by 1.5–2%), so the median across forks is the reported number and the min–max range
  * is the honest error bar — never a single fork's value.
  */
case class WorkloadResult(workload: String, forks: List[ForkMeasurement]) {
  require(forks.nonEmpty, "a workload result needs at least one fork")

  /** Median ns/edge across forks. */
  def medianNsPerEdge: Double = Bench.median(forks.map(_.medianNsPerEdge))

  /** Fastest fork's median ns/edge. */
  def minNsPerEdge: Double = forks.map(_.medianNsPerEdge).min

  /** Slowest fork's median ns/edge. */
  def maxNsPerEdge: Double = forks.map(_.medianNsPerEdge).max

  /** Clock edges per wall-clock second, from the median ns/edge. */
  def edgesPerSecond: Double = 1e9 / medianNsPerEdge

  /** Signal transitions per clock edge (`--profile` mode only), median across forks. */
  def transitionsPerEdge: Double = Bench.median(forks.map(_.transitionsPerEdge))

  /** Wall-clock nanoseconds per signal transition (`--profile` mode only). */
  def nsPerTransition: Double =
    if (transitionsPerEdge > 0) medianNsPerEdge / transitionsPerEdge else 0.0
}

/** Best-effort description of the machine a benchmark ran on, for the console report and `bench/results.md`. */
object MachineInfo {

  /** CPU model with processor count, e.g. `AMD EPYC 9D25 126-Core Processor x4`. */
  def cpu: String = {
    val model = readFirst("/proc/cpuinfo", "model name")
      .orElse(sysctl("machdep.cpu.brand_string"))
      .orElse(readWhole("/proc/device-tree/model"))
      .orElse(lscpuModel)
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

  /** Whole file content, trimmed — device-tree files carry no newline and a trailing NUL, both trimmed away. */
  private def readWhole(file: String): Option[String] =
    try {
      val src = scala.io.Source.fromFile(file)
      try Some(src.mkString.trim).filter(_.nonEmpty)
      finally src.close()
    } catch { case _: Exception => None }

  private def run(cmd: Seq[String]): Option[String] =
    try {
      val out = new StringBuilder
      val code = scala.sys.process
        .Process(cmd)
        .!(scala.sys.process.ProcessLogger(s => { out.append(s).append("\n"); () }))
      if (code == 0) Some(out.toString).filter(_.nonEmpty) else None
    } catch { case _: Exception => None }

  private def sysctl(key: String): Option[String] =
    run(Seq("sysctl", "-n", key)).map(_.trim).filter(_.nonEmpty)

  /** `Model name` field from `lscpu`, e.g. `Cortex-A76` on ARM Linux without `/proc/cpuinfo` entries. */
  private def lscpuModel: Option[String] =
    run(Seq("lscpu"))
      .flatMap(_.linesIterator.find(_.startsWith("Model name:")).map(_.split(":", 2)(1).trim))
      .filter(_.nonEmpty)
}

/** The JDK-major pin for `--write-results`: recorded rows must come from the JDK `.sdkmanrc` pins, so a row's JVM
  * column stays comparable with the rest of the log.
  */
private[bench] object JdkPin {

  /** Major version of a `java.version`-style string: its leading digits. */
  def majorOf(version: String): Option[Int] =
    version.takeWhile(_.isDigit) match {
      case s if s.nonEmpty => s.toIntOption
      case _ => None
    }

  /** Major version pinned by the `java=` line of `.sdkmanrc` content. */
  def pinnedMajor(sdkmanrc: String): Option[Int] =
    sdkmanrc.linesIterator.find(_.startsWith("java=")).flatMap(line => majorOf(line.stripPrefix("java=")))

  def check(currentVersion: String, sdkmanrc: String): Either[String, Unit] =
    (majorOf(currentVersion), pinnedMajor(sdkmanrc)) match {
      case (Some(current), Some(pinned)) if current == pinned => Right(())
      case (Some(current), Some(pinned)) =>
        Left(s"JDK major $current does not match the .sdkmanrc pin $pinned")
      case _ => Left("cannot determine the JDK major or the .sdkmanrc pin")
    }

  /** Check the live JVM against the repo's `.sdkmanrc`. */
  def checkLive(): Either[String, Unit] =
    try {
      val content = java.nio.file.Files.readString(java.nio.file.Paths.get(".sdkmanrc"))
      check(sys.props.getOrElse("java.version", ""), content)
    } catch { case e: Exception => Left(s"cannot read .sdkmanrc: ${e.getMessage}") }
}

object Bench {

  /** The gate-level engine, Bench's default choice. */
  val gateEngineFactory: Workload.EngineFactory =
    (circuit, conf, portNames) => GateEngine(GateProcessor.setup(circuit, conf, portNames))

  case class Options(
      workload: Option[String] = None,
      forks: Option[Int] = None,
      warmupSeconds: Double = 3.0,
      measureSeconds: Double = 3.0,
      profile: Boolean = false,
      writeResults: Boolean = false
  ) {

    /** Forks per workload: explicit `--forks`, else 7 for `--write-results`, else 5. */
    def resolvedForks: Int = forks.getOrElse(if (writeResults) 7 else 5)
  }

  /** Median of a non-empty sample: the middle value, or the mean of the two middle values. */
  def median(values: Seq[Double]): Double = {
    require(values.nonEmpty, "median of no values")
    val sorted = values.sorted
    val n = sorted.size
    if (n % 2 == 1) sorted(n / 2) else (sorted(n / 2 - 1) + sorted(n / 2)) / 2.0
  }

  def main(args: Array[String]): Unit = {
    val opts = parse(args.toList)
    if (opts.writeResults) JdkPin.checkLive() match {
      case Left(err) =>
        println(s"refusing --write-results: $err")
        sys.exit(1)
      case Right(_) => ()
    }
    val selected = opts.workload match {
      case Some(name) =>
        Workloads.named(name) match {
          case Some(w) => List(w)
          case None =>
            println(s"unknown workload '$name'; available: ${Workloads.all.map(_.name).mkString(", ")}")
            sys.exit(1)
            Nil
        }
      case None => Workloads.all
    }
    val results =
      try selected.map(runForked(_, opts))
      catch {
        case e: Exception =>
          println(s"benchmark failed: ${e.getMessage}")
          sys.exit(1)
          Nil
      }
    println(Report.text(results, opts))
    if (opts.writeResults) ResultsLog.write(results) match {
      case Left(err) =>
        println(s"refusing to write bench/results.md: $err")
        sys.exit(1)
      case Right(path) => println(s"appended ${results.size} rows to $path")
    }
  }

  /** Run a workload in fresh JVMs, sequentially so forks never contend for CPU, and aggregate their medians.
    * `classpath` defaults to this JVM's classpath, which is the benchmark classpath when Bench runs forked from sbt;
    * callers running inside another JVM (e.g. tests under `Test / fork := false`) pass their own.
    */
  def runForked(
      workload: Workload,
      opts: Options,
      classpath: String = sys.props("java.class.path")
  ): WorkloadResult =
    WorkloadResult(workload.name, (1 to opts.resolvedForks).map(_ => launchFork(workload, opts, classpath)).toList)

  /** Launch one benchmark JVM (same `java`, given classpath, G1 regardless of the parent's GC) and parse its result. */
  private def launchFork(workload: Workload, opts: Options, classpath: String): ForkMeasurement = {
    val javaBin = java.nio.file.Paths.get(sys.props("java.home"), "bin", "java").toString
    val cmd = Seq(
      javaBin,
      "-XX:+UseG1GC",
      "-cp",
      classpath,
      "bench.BenchFork",
      workload.name,
      opts.warmupSeconds.toString,
      opts.measureSeconds.toString
    ) ++ (if (opts.profile) Seq("--profile") else Seq.empty)
    val out = new StringBuilder
    val code = scala.sys.process
      .Process(cmd)
      .!(
        scala.sys.process.ProcessLogger(
          line => { out.append(line).append("\n"); () },
          line => { System.err.println(line); () }
        )
      )
    if (code != 0) throw new RuntimeException(s"benchmark fork for '${workload.name}' exited with code $code")
    out.toString.linesIterator.find(_.startsWith(BenchFork.resultPrefix)) match {
      case Some(line) => BenchFork.parseResult(line)
      case None =>
        throw new RuntimeException(s"benchmark fork for '${workload.name}' printed no result line")
    }
  }

  private def parse(args: List[String]): Options = {
    def loop(rest: List[String], o: Options): Options = rest match {
      case "--workload" :: name :: tail => loop(tail, o.copy(workload = Some(name)))
      case "--forks" :: n :: tail => loop(tail, o.copy(forks = Some(n.toInt)))
      case "--warmup-seconds" :: s :: tail => loop(tail, o.copy(warmupSeconds = s.toDouble))
      case "--measure-seconds" :: s :: tail => loop(tail, o.copy(measureSeconds = s.toDouble))
      case "--profile" :: tail => loop(tail, o.copy(profile = true))
      case "--write-results" :: tail => loop(tail, o.copy(writeResults = true))
      case "--help" :: _ =>
        println("""usage: sbt "bench [--workload <name>] [--forks <n>]
          |                [--warmup-seconds <s>] [--measure-seconds <s>] [--profile] [--write-results]"
          |
          |  --workload       only run this workload (default: all registered)
          |  --forks          fresh JVMs per workload (default 5, 7 with --write-results)
          |  --warmup-seconds fixed warmup time per JVM (default 3.0)
          |  --measure-seconds measurement time per JVM; each JVM reports its
          |                median ns/edge across iterations (default 3.0)
          |  --profile       count signal transitions via engine watch() and report
          |                events per edge (adds observer overhead)
          |  --write-results append the full suite's rows to bench/results.md
          |                (only on master with a clean tree, on the .sdkmanrc
          |                JDK; cannot be combined with --workload or --profile)""".stripMargin)
        sys.exit(0)
      case bad :: _ =>
        println(s"unknown argument '$bad'; see --help")
        sys.exit(1)
      case Nil => o
    }
    val opts =
      try loop(args, Options())
      catch {
        case _: NumberFormatException =>
          println("a numeric flag got a non-numeric value; see --help")
          sys.exit(1)
          Options()
      }
    if (opts.forks.exists(_ < 1)) {
      println("--forks must be at least 1")
      sys.exit(1)
    }
    if (opts.warmupSeconds < 0 || opts.measureSeconds < 0) {
      println("--warmup-seconds and --measure-seconds must not be negative")
      sys.exit(1)
    }
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

/** The log of `master`'s performance in `bench/results.md`: one appended row per workload per run. */
private[bench] object ResultsLog {
  val relativePath = "bench/results.md"

  /** Check the write preconditions; on success the short commit the new rows will cite. */
  def guard(): Either[String, String] = {
    def git(args: String*): Either[String, String] =
      try {
        val out = new StringBuilder
        val err = new StringBuilder
        val code = scala.sys.process
          .Process(Seq("git") ++ args)
          .!(
            scala.sys.process
              .ProcessLogger(s => { out.append(s).append("\n"); () }, s => { err.append(s).append("\n"); () })
          )
        if (code == 0) Right(out.toString.trim) else Left(err.toString.trim)
      } catch { case e: Exception => Left(e.getMessage) }
    for {
      branch <- git("rev-parse", "--abbrev-ref", "HEAD").left.map(_ => "cannot determine the current branch")
      _ <- Either.cond(branch == "master", (), s"HEAD is on '$branch', not 'master'")
      status <- git("status", "--porcelain", "--", ".", s":!$relativePath").left
        .map(_ => "cannot read the working tree status")
      _ <- Either.cond(
        status.isEmpty,
        (),
        s"working tree has changes outside $relativePath:\n$status"
      )
      commit <- git("rev-parse", "--short", "HEAD").left.map(_ => "cannot determine HEAD's commit")
    } yield commit
  }

  /** Append one row per result to the log file, which ships in the repo. */
  def write(results: List[WorkloadResult]): Either[String, java.nio.file.Path] =
    guard() match {
      case Left(err) => Left(err)
      case Right(commit) =>
        val path = java.nio.file.Paths.get(relativePath)
        try {
          java.nio.file.Files.writeString(
            path,
            Report.rows(results, java.time.LocalDate.now().toString, commit),
            java.nio.file.StandardOpenOption.APPEND
          )
          Right(path)
        } catch { case e: Exception => Left(e.getMessage) }
    }
}

private[bench] object Report {

  /** One log row per result: the median across forks and its min–max range, each carrying its own date, commit, JVM and
    * CPU.
    */
  def rows(results: List[WorkloadResult], date: String, commit: String): String =
    results
      .map { r =>
        f"| $date | $commit | ${r.workload} | ${r.forks.size} | ${r.medianNsPerEdge}%.1f | ${r.minNsPerEdge}%.1f | ${r.maxNsPerEdge}%.1f | ${r.edgesPerSecond}%.0f | ${MachineInfo.jvm} | ${MachineInfo.cpu} |"
      }
      .mkString("", "\n", "\n")

  def text(results: List[WorkloadResult], opts: Bench.Options): String = {
    val sb = new StringBuilder
    sb.append("machine:\n")
    MachineInfo.describe.linesIterator.foreach(l => sb.append(s"  $l\n"))
    sb.append(
      f"${"workload"}%-10s ${"forks"}%5s ${"median ns/edge"}%14s ${"min"}%12s ${"max"}%12s ${"edges/s"}%12s"
    )
    if (opts.profile) sb.append(f" ${"ev/edge"}%9s ${"ns/ev"}%9s")
    sb.append("\n")
    results.foreach { r =>
      sb.append(
        f"${r.workload}%-10s ${r.forks.size}%5d ${r.medianNsPerEdge}%14.1f ${r.minNsPerEdge}%12.1f ${r.maxNsPerEdge}%12.1f " +
          f"${r.edgesPerSecond}%12.0f"
      )
      if (opts.profile) sb.append(f" ${r.transitionsPerEdge}%9.1f ${r.nsPerTransition}%9.1f")
      sb.append("\n")
    }
    sb.toString
  }

}
