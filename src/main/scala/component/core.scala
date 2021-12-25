package component

import component.BuilderDSL._
import core._

def nand(in1: Port, in2: Port)(using env: BuilderEnv): Port = newComponent {
  val nand = new NAND
  env.add(nand)
  env.wire(in1, nand.in1)
  env.wire(in2, nand.in2)
  nand.out
}

def flipflop(set: Port, reset: Port)(using env: BuilderEnv): (Port, Port) = newComponent {
  val ff = new Flipflop
  env.add(ff)
  env.wire(set, ff.set)
  env.wire(reset, ff.reset)
  (ff.q, ff.nq)
}

def clock(freq: Int)(using env: BuilderEnv): Port = newComponent {
  val clock = new Clock(freq)
  env.add(clock)
  clock.out
}

def posEdge(in: Port)(using env: BuilderEnv): Port = newComponent {
  val posEdge = new PosEdge
  env.add(posEdge)
  env.wire(in, posEdge.in)
  posEdge.out
}
