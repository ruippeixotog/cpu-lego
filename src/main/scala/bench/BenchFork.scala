package bench

/** Entry point for a single benchmark fork, launched by [[Bench]] with the same `java` and classpath but G1: it warms
  * the workload up for a fixed time, measures for a fixed time, and prints one result line for the parent to parse.
  * Nothing else may print to stdout — the parent reads only the result line.
  */
object BenchFork {

  val resultPrefix = "BENCH_FORK_RESULT"

  def main(args: Array[String]): Unit = {
    val (workload, warmupSeconds, measureSeconds, profile) = parse(args.toList)
    val measurement = measure(workload, warmupSeconds, measureSeconds, profile)
    println(formatResult(measurement))
  }

  /** Warm up for `warmupSeconds`, then measure for `measureSeconds` (at least one iteration) and report the median
    * ns/edge across measured iterations. In `--profile` mode the transition watch is installed after warmup, so only
    * measured iterations are counted.
    */
  def measure(
      workload: Workload,
      warmupSeconds: Double,
      measureSeconds: Double,
      profile: Boolean = false,
      engineFactory: Workload.EngineFactory = Bench.gateEngineFactory
  ): ForkMeasurement = {
    val run = workload.build(engineFactory)

    val warmT0 = System.nanoTime()
    while ((System.nanoTime() - warmT0).toDouble < warmupSeconds * 1e9) run.iterate()

    val countTransitions =
      if (profile) {
        var count = 0L
        run.ports.foreach(port => run.engine.watch(port)(_ => count += 1))
        () => count
      } else () => 0L

    val samples = scala.collection.mutable.ArrayBuffer.empty[Double]
    var totalEdges = 0L
    var iters = 0
    val t0 = System.nanoTime()
    while (iters == 0 || (System.nanoTime() - t0).toDouble < measureSeconds * 1e9) {
      val it0 = System.nanoTime()
      val edges = run.iterate()
      val durNs = System.nanoTime() - it0
      require(edges > 0, s"benchmark iteration for '${workload.name}' drove no clock edges")
      samples += durNs.toDouble / edges
      totalEdges += edges
      iters += 1
    }
    val transitions = countTransitions()
    ForkMeasurement(
      Bench.median(samples.toSeq),
      iters,
      totalEdges,
      if (profile && totalEdges > 0) transitions.toDouble / totalEdges else 0.0
    )
  }

  def formatResult(m: ForkMeasurement): String =
    s"$resultPrefix ${m.medianNsPerEdge} ${m.iterations} ${m.totalEdges} ${m.transitionsPerEdge}"

  def parseResult(line: String): ForkMeasurement = {
    val parts = line.split("\\s+")
    require(
      parts.length == 5 && parts(0) == resultPrefix,
      s"malformed benchmark fork result line: '$line'"
    )
    ForkMeasurement(parts(1).toDouble, parts(2).toInt, parts(3).toLong, parts(4).toDouble)
  }

  private def parse(args: List[String]): (Workload, Double, Double, Boolean) = args match {
    case name :: warmup :: measure :: rest =>
      val profile = rest match {
        case List("--profile") => true
        case Nil => false
        case _ =>
          println(s"unknown benchmark fork arguments: ${rest.mkString(" ")}")
          sys.exit(1)
          false
      }
      Workloads.named(name) match {
        case Some(w) =>
          try (w, warmup.toDouble, measure.toDouble, profile)
          catch {
            case _: NumberFormatException =>
              println("benchmark fork warmup/measure seconds must be numbers")
              sys.exit(1)
              (w, 0.0, 0.0, profile)
          }
        case None =>
          println(s"unknown workload '$name'")
          sys.exit(1)
          (Sap1Workload, 0.0, 0.0, profile)
      }
    case _ =>
      println("usage: bench.BenchFork <workload> <warmupSeconds> <measureSeconds> [--profile]")
      sys.exit(1)
      (Sap1Workload, 0.0, 0.0, false)
  }
}
