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

    "count signal transitions in profiling mode" in {
      val result = Bench.run(Sap1Workload, Bench.Options(profile = true, fixedIterations = Some(1), warmup = 0))
      result.transitions must be_>(0L)
      result.transitionsPerEdge must be_>(0.0)
    }
  }

  "WorkloadResult" should {
    "derive the reported metrics" in {
      // 264 edges in 0.264 s = 1000 edges/s, 1 ms per edge
      val r = WorkloadResult("w", 2, 264L, 264000000L, 792L)
      r.edgesPerSecond must beEqualTo(1000.0)
      r.nsPerEdge must beEqualTo(1000000.0)
      r.transitionsPerEdge must beEqualTo(3.0)
    }
  }

  "Report" should {
    "ship the log header with no rows and no Machine section" in {
      val rendered = Report.header
      rendered must contain("| date | commit | workload | iters | edges/s | ns/edge | JVM | CPU |")
      rendered.contains("## Machine") must beFalse
      rendered.contains("| sap1 |") must beFalse
    }

    "append one row per workload, preserving existing rows" in {
      val results = List(
        WorkloadResult("sap1", 4, 528L, 2000000000L, 0L),
        WorkloadResult("other", 2, 200L, 500000000L, 0L)
      )
      val rendered = Report.append(Report.header, results, "2026-11-03", "abc1234")
      rendered must contain("| 2026-11-03 | abc1234 | sap1 | 4 | 264 | 3787878.8 |")
      rendered must contain("| 2026-11-03 | abc1234 | other | 2 | 400 | 2500000.0 |")
      // each row carries its own JVM and CPU; the Machine section is gone
      rendered must contain(MachineInfo.jvm)
      rendered must contain(MachineInfo.cpu)
      rendered.contains("## Machine") must beFalse
      val appended = Report.append(rendered, List(results.head), "2026-11-04", "def5678")
      appended must contain("| 2026-11-03 | abc1234 | sap1 | 4 |")
      appended must contain("| 2026-11-04 | def5678 | sap1 | 4 |")
    }

    "append cleanly to content without a trailing newline, and to an empty file" in {
      val row = Report.append(
        "header without newline",
        List(WorkloadResult("sap1", 1, 131L, 1000000000L, 0L)),
        "2026-11-03",
        "abc1234"
      )
      row must contain("header without newline\n| 2026-11-03 | abc1234 | sap1 | 1 | 131 |")
      val fresh = Report.append("", List(WorkloadResult("sap1", 1, 131L, 1000000000L, 0L)), "2026-11-03", "abc1234")
      fresh must startWith(Report.header)
      fresh must contain("| 2026-11-03 | abc1234 | sap1 | 1 | 131 |")
    }
  }
}
