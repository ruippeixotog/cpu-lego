package bench

import testkit.BaseSpec
import util.Implicits.*

class BenchSpec extends BaseSpec {

  "Workloads" should {
    "register the sap1 workload" in {
      Workloads.named("sap1") must beSome(Sap1Workload)
      Workloads.all.map(_.name) must contain("sap1")
    }
  }

  "Sap1Workload" should {
    "run the demo program to halt with the expected output" in {
      val run = Sap1Workload.build(Bench.gateEngineFactory).asInstanceOf[Sap1Run]
      val edges = run.iterate()
      edges must beEqualTo(131L)
      // 10 + 14 + 18 - 20 = 22, the same assertion SAP1App prints
      run.engine.get(run.outBus).map(_.getOrElse(false)).toSignedInt must beEqualTo(22)
    }

    "return a deterministic edge count per iteration" in {
      val run = Sap1Workload.build(Bench.gateEngineFactory).asInstanceOf[Sap1Run]
      val first = run.iterate()
      val second = run.iterate()
      second must beEqualTo(first)
    }
  }

  "Bench.median" should {
    "take the middle value of an odd count" in {
      Bench.median(Seq(3.0, 1.0, 2.0)) must beEqualTo(2.0)
    }

    "average the two middle values of an even count" in {
      Bench.median(Seq(4.0, 1.0, 3.0, 2.0)) must beEqualTo(2.5)
    }
  }

  "BenchFork.measure" should {
    "report a positive median ns/edge over at least one iteration" in {
      val m = BenchFork.measure(Sap1Workload, warmupSeconds = 0.05, measureSeconds = 0.1)
      m.medianNsPerEdge must be_>(0.0)
      m.iterations must be_>=(1)
      m.totalEdges must beEqualTo(m.iterations * 131L)
    }

    "count signal transitions in profiling mode" in {
      val m = BenchFork.measure(Sap1Workload, warmupSeconds = 0.0, measureSeconds = 0.1, profile = true)
      m.transitionsPerEdge must be_>(0.0)
    }

    "round-trip through the result line" in {
      val m = ForkMeasurement(1234.5, 3, 393L, 7.5)
      BenchFork.parseResult(BenchFork.formatResult(m)) must beEqualTo(m)
    }
  }

  "Bench.runForked" should {
    "aggregate fresh JVMs into a median and range" in {
      // The fork classpath is this class's loader chain (project entries first) plus this JVM's
      // java.class.path: in a forked test JVM the system loader is not a URLClassLoader, so the
      // chain yields nothing and java.class.path — the full test classpath there — carries the
      // project classes; inside sbt's JVM java.class.path holds sbt's own jars, so the loader
      // chain comes first and sbt's entries can only add, never shadow, project classes.
      def loaderUrls(cl: ClassLoader): Seq[java.net.URL] = cl match {
        case null => Seq.empty
        case u: java.net.URLClassLoader => u.getURLs.toSeq ++ loaderUrls(u.getParent)
        case other => loaderUrls(other.getParent)
      }
      val classpath = (
        loaderUrls(getClass.getClassLoader).map(u => java.nio.file.Paths.get(u.toURI).toString)
          ++ sys.props("java.class.path").split(java.io.File.pathSeparator).toSeq
      ).distinct.mkString(java.io.File.pathSeparator)
      val result = Bench.runForked(
        Sap1Workload,
        Bench.Options(forks = Some(2), warmupSeconds = 0.05, measureSeconds = 0.1),
        classpath
      )
      result.forks.size must beEqualTo(2)
      result.medianNsPerEdge must be_>(0.0)
      result.minNsPerEdge must be_<=(result.medianNsPerEdge)
      result.maxNsPerEdge must be_>=(result.medianNsPerEdge)
      result.edgesPerSecond must beEqualTo(1e9 / result.medianNsPerEdge)
    }
  }

  "WorkloadResult" should {
    "derive median, range and edges/s from its forks" in {
      val r = WorkloadResult(
        "w",
        List(
          ForkMeasurement(1000.0, 2, 262L, 0.0),
          ForkMeasurement(1200.0, 2, 262L, 0.0),
          ForkMeasurement(1100.0, 2, 262L, 0.0)
        )
      )
      r.medianNsPerEdge must beEqualTo(1100.0)
      r.minNsPerEdge must beEqualTo(1000.0)
      r.maxNsPerEdge must beEqualTo(1200.0)
      r.edgesPerSecond must beEqualTo(1e9 / 1100.0)
    }
  }

  "Report" should {
    "format one row per workload with median and range" in {
      val results = List(
        WorkloadResult("sap1", List(ForkMeasurement(1000.0, 2, 262L, 0.0), ForkMeasurement(1200.0, 2, 262L, 0.0))),
        WorkloadResult("other", List(ForkMeasurement(500.0, 2, 200L, 0.0)))
      )
      val rendered = Report.rows(results, "2026-11-03", "abc1234")
      rendered must contain("| 2026-11-03 | abc1234 | sap1 | 2 | 1100.0 | 1000.0 | 1200.0 | 909091 |")
      rendered must contain("| 2026-11-03 | abc1234 | other | 1 | 500.0 | 500.0 | 500.0 | 2000000 |")
      rendered must contain(MachineInfo.jvm)
      rendered must contain(MachineInfo.cpu)
      rendered.contains("## Machine") must beFalse
    }
  }

  "JdkPin" should {
    "accept the pinned major and reject others" in {
      JdkPin.check("25.0.4.1", "java=25.0.4.1-tem") must beRight
      JdkPin.check("25", "java=25.0.4.1-tem") must beRight
      JdkPin.check("21.0.2", "java=25.0.4.1-tem") must beLeft
      JdkPin.check("25.0.4.1", "scala=3.6.4") must beLeft
    }
  }

  "Bench.Options" should {
    "default to 5 forks, 7 for --write-results, explicit wins" in {
      Bench.Options().resolvedForks must beEqualTo(5)
      Bench.Options(writeResults = true).resolvedForks must beEqualTo(7)
      Bench.Options(writeResults = true, forks = Some(3)).resolvedForks must beEqualTo(3)
    }
  }
}
