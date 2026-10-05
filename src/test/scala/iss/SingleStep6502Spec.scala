package iss

import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragment

/** Runs the SingleStepTests 6502 suites against [[Cpu6502]]: for every implemented opcode the initial registers and RAM
  * are loaded, the instruction's cycles are stepped, and the final registers, RAM and the per-cycle
  * `[address, value, read/write]` list are compared.
  *
  * Test data is fetched on first use through [[util.ExternalAssets]] and cached; it is never vendored. When an asset is
  * unavailable the example skips with a clear message. By default every 32nd test per opcode runs; set
  * `CPU_LEGO_SINGLESTEP_FULL=1` for the full set (see README).
  */
class SingleStep6502Spec extends Specification {

  "SingleStepTests 6502 suites" should {

    Fragment.foreach(SingleStepRunner.implementedOpcodes) { op =>
      val hex = f"$op%02x"
      s"pass for opcode $hex" in {
        SingleStepRunner.load(op) match {
          case None =>
            skipped(
              s"SingleStepTests asset ${SingleStepRunner.assetName(op)} unavailable: " +
                "no network access and not cached (see README 'External test assets')"
            )
          case Some(tests) =>
            tests.flatMap(SingleStepRunner.run).take(5) match {
              case Nil => success
              case failures =>
                failure(s"${tests.length} sampled tests, first failures:\n" + failures.mkString("\n"))
            }
        }
      }
    }
  }
}
