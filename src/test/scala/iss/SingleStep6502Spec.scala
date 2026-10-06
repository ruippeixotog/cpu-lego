package iss

import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragment

class SingleStep6502Spec extends Specification {

  "SingleStepTests 6502 suites" should {

    Fragment.foreach(SingleStepRunner.implementedOpcodes) { op =>
      val hex = f"$op%02x"
      s"pass for opcode $hex" in {
        SingleStepRunner.load(op) match {
          case Left(cause) =>
            skipped(
              s"SingleStepTests asset ${SingleStepRunner.assetName(op)} unavailable: $cause " +
                "(see README 'External test assets')"
            )
          case Right(tests) =>
            val failures = tests.flatMap(SingleStepRunner.run).take(5)
            if (failures.isEmpty) success
            else failure(s"${tests.length} sampled tests, first failures:\n" + failures.mkString("\n"))
        }
      }
    }
  }
}
