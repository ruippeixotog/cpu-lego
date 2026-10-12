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
      val run = Sap1Workload.build().asInstanceOf[Sap1Run]
      val ticks = run.iterate()
      ticks must be_>(0L)
      // 10 + 14 + 18 - 20 = 22, the same assertion SAP1App prints
      run.engine.get(run.outBus).map(_.getOrElse(false)).toSignedInt must beEqualTo(22)
    }

    "advance by a deterministic tick count per iteration" in {
      val run = Sap1Workload.build().asInstanceOf[Sap1Run]
      val first = run.iterate()
      val second = run.iterate()
      second must beEqualTo(first)
    }

    "count signal transitions in profiling mode" in {
      val result = Bench.run(Sap1Workload, Bench.Options(profile = true, fixedIterations = Some(1), warmup = 0))
      result.transitions must be_>(0L)
      result.transitionsPerTick must be_>(0.0)
    }
  }

  "WorkloadResult" should {
    "derive the reported metrics" in {
      // 2e6 ticks in 1 s = 2 MHz simulated at 1 tick = 1 ns
      val r = WorkloadResult("w", "d", 2, 2000000L, 1000000000L, 6000000L)
      r.ticksPerSecond must beEqualTo(2000000.0)
      r.simulatedMHz must beEqualTo(2.0)
      r.nsPerTick must beEqualTo(500.0)
      r.transitionsPerTick must beEqualTo(3.0)
      r.deviceCallbackNs must beEqualTo(0L)
    }
  }
}
