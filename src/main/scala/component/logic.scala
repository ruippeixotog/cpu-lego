package component

import component.BuilderAPI._
import core._

def not(in: Port): Spec[Port] = newSpec {
  nand(in, in)
}

def and(in1: Port, in2: Port): Spec[Port] = newSpec {
  not(nand(in1, in2))
}

def andM(ins: Port*): Spec[Port] = newSpec {
  multi(and)(ins: _*)
}

def nandM(ins: Port*): Spec[Port] = newSpec {
  not(multi(and)(ins: _*))
}

def or(in1: Port, in2: Port): Spec[Port] = newSpec {
  nand(not(in1), not(in2))
}

def orM(ins: Port*): Spec[Port] = newSpec {
  multi(or)(ins: _*)
}

def nor(in1: Port, in2: Port): Spec[Port] = newSpec {
  not(or(in1, in2))
}

def norM(ins: Port*): Spec[Port] = newSpec {
  not(multi(or)(ins: _*))
}

def xor(in1: Port, in2: Port): Spec[Port] = newSpec {
  val nand12 = nand(in1, in2)
  nand(nand(in1, nand12), nand(in2, nand12))
}

def xorM(ins: Port*): Spec[Port] = newSpec {
  multi(xor)(ins: _*)
}

def xnor(in1: Port, in2: Port): Spec[Port] = newSpec {
  not(xor(in1, in2))
}

def xnorM(ins: Port*): Spec[Port] = newSpec {
  not(multi(xor)(ins: _*))
}
