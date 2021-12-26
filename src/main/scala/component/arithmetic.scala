package component

import component.BuilderAPI._
import core._

def fullAdder(in1: Port, in2: Port, in3: Port): Spec[(Port, Port)] = newSpec {
  val carry = multi(or)(and(in1, in2), and(in1, in3), and(in2, in3))
  val out = multi(xor)(in1, in2, in3)
  (out, carry)
}

def binaryAdder(ins1: Bus, ins2: Bus, carryIn: Port = Low): Spec[(Bus, Port)] = newSpec {
  val (outs, carry) = ins1.zip(ins2).foldLeft((List.empty[Port], carryIn)) { case ((prevOuts, prevCarry), (a, b)) =>
    val (out, carry) = fullAdder(a, b, prevCarry)
    (out :: prevOuts, carry)
  }
  (outs.reverse.toVector, carry)
}

def addSub(ins1: Bus, ins2: Bus, sub: Port): Spec[Bus] = newSpec {
  binaryAdder(ins1, ins2.map(xor(_, sub)), sub)._1
}
