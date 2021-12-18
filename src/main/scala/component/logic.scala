package component

import component.BuilderDSL._
import core._

def not(in: Port)(using BuilderEnv): Port = nand(in, in)

def and(in1: Port, in2: Port)(using BuilderEnv): Port = not(nand(in1, in2))

def or(in1: Port, in2: Port)(using BuilderEnv): Port = nand(not(in1), not(in2))

def nor(in1: Port, in2: Port)(using BuilderEnv): Port = not(or(in1, in2))

def xor(in1: Port, in2: Port)(using BuilderEnv): Port = {
  val nand12 = nand(in1, in2)
  nand(nand(in1, nand12), nand(in2, nand12))
}

def xnor(in1: Port, in2: Port)(using BuilderEnv): Port = not(xor(in1, in2))
