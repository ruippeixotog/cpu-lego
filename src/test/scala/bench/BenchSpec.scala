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
    "merge new rows into a previous table by workload name" in {
      val old = List(
        WorkloadResult("sap1", 3, 396L, 1000000000L, 0L),
        WorkloadResult("other", 2, 200L, 500000000L, 0L)
      )
      val previous = Report.markdown(old)
      val fresh = List(WorkloadResult("sap1", 4, 528L, 2000000000L, 0L))
      val merged = Report.markdown(fresh, Some(previous))
      merged must contain("| sap1 | 4 | 528 |")
      merged must contain("| other | 2 | 200 |")
      merged must not(contain("| sap1 | 3 | 396 |"))
    }
  }
}
