package component

import component.BuilderDSL._
import core._

def fullAdder(in1: Port, in2: Port, in3: Port)(using BuilderEnv): (Port, Port) = {
  val carry = multi(or)(and(in1, in2), and(in1, in3), and(in2, in3))
  val out = multi(xor)(in1, in2, in3)
  (out, carry)
}

def binaryAdder(ins1: Seq[Port], ins2: Seq[Port])(using BuilderEnv): (Seq[Port], Port) = {
  val (outs, carry) = ins1.zip(ins2).foldLeft[(List[Port], Port)]((Nil, Low)) {
    case ((prevOuts, prevCarry), (a, b)) =>
      val (out, carry) = fullAdder(a, b, prevCarry)
      (out :: prevOuts, carry)
  }
  (outs.reverse, carry)
}
