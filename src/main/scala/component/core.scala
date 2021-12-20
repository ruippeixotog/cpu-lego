package component

import component.BuilderDSL._
import core._

def nand(in1: Port, in2: Port)(using env: BuilderEnv): Port = {
  val nand = new NAND
  env.add(nand)
  env.wire(in1, nand.in1)
  env.wire(in2, nand.in2)
  nand.out
}

def clock(freq: Int)(using env: BuilderEnv): Port = {
  val clock = new Clock(freq)
  env.add(clock)
  clock.out
}

def posEdge(in: Port)(using env: BuilderEnv): Port = {
  val posEdge = new PosEdge
  env.add(posEdge)
  env.wire(in, posEdge.in)
  posEdge.out
}
