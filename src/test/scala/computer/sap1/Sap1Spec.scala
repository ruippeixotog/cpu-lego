package computer.sap1

import computer.sap1.Instr.*
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
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
      sap1.run.get(sap1.out).sequence.map(_.toSignedInt) must beEqualTo(expected)
    }
  }
}
