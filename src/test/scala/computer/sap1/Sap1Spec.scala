package computer.sap1

import computer.sap1.Instr.*
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import simulator.GateProcessor
import testkit.*
import util.Implicits.*

class Sap1Spec extends BaseSpec {

  val genDataAddr: Gen[Int] = Gen.choose(10, 15)
  val genData: Gen[Data] = Gen.choose(0, 32).map(Data.apply)

  val genInstr: Gen[Instr] = Gen.oneOf(
    genDataAddr.map(LDA.apply),
    genDataAddr.map(ADD.apply),
    genDataAddr.map(SUB.apply),
    Gen.const(OUT)
  )

  val genProg: Gen[List[MemEntry]] = for {
    instrs <- Gen.listOfN(7, genInstr)
    data <- Gen.listOfN(16 - instrs.length - 1, genData)
  } yield instrs :+ HLT :++ data

  given arbProg: Arbitrary[List[MemEntry]] = Arbitrary(genProg)

  "A SAP-1" should {

    "run the code provided" in forAll { (prog: List[MemEntry]) =>
      def getData(addr: Int): Int = prog(addr) match {
        case instr: Instr => instr.repr.toInt
        case Data(value) => value
      }

      val expected = prog
        .foldLeft[(Option[Int], Option[Int])](None, None) {
          case ((_, out), LDA(addr)) => (Some(getData(addr)), out)
          case ((Some(a), out), ADD(addr)) => (Some(a + getData(addr)), out)
          case ((Some(a), out), SUB(addr)) => (Some(a - getData(addr)), out)
          case ((Some(a), _), OUT) => (Some(a), Some(a))
          case ((a, out), _) => (a, out)
        }
        ._2

      val sap1 = SAP1(prog)
      runFunctional(sap1, prog).get(sap1.out).sequence.map(_.toSignedInt) must beEqualTo(expected)
    }
  }

  /** Functional harness for the SAP-1 wiring: program the RAM and toggle the clock to quiescence until the program
    * halts. Deterministic and fast; the live equivalent is computer.sap1.SAP1.run, the demo.
    */
  private def runFunctional(sap1: SAP1, prog: List[MemEntry]): GateProcessor = {
    def load(sim: GateProcessor, prog: List[MemEntry], addr: Int = 0): GateProcessor =
      prog match {
        case entry :: rest =>
          load(
            sim
              .set(sap1.ramIn.prog, true)
              .set(sap1.ramIn.addr, addr.toBoolVec(4))
              .set(
                sap1.ramIn.data,
                entry match {
                  case instr: Instr => instr.repr
                  case Data(value) => value.toBoolVec(8)
                }
              )
              .run()
              .set(sap1.ramIn.write, true)
              .run()
              .set(sap1.ramIn.write, false)
              .run(),
            rest,
            addr + 1
          )
        case Nil =>
          sim.set(sap1.ramIn.prog, false).run()
      }

    var sim = load(
      GateProcessor
        .setup(sap1.comp)
        .set(sap1.clkSig, false)
        .set(sap1.clr, false)
        .run()
        .set(sap1.clr, true)
        .run(),
      prog
    )
    while (sim.get(sap1.hlt) != Some(true))
      sim = sim.toggle(sap1.clkSig).run().toggle(sap1.clkSig).run()
    sim
  }
}
