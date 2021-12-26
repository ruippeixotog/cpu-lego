package component

import component.BuilderAPI._
import core._

def nand(in1: Port, in2: Port)(using env: BuilderEnv): Port = newSpec {
  val nand = new NAND
  env.add(nand)
  env.wire(in1, nand.in1)
  env.wire(in2, nand.in2)
  nand.out
}

def flipflop(set: Port, reset: Port)(using env: BuilderEnv): (Port, Port) = newSpec {
  val ff = new FlipFlop
  env.add(ff)
  env.wire(set, ff.set)
  env.wire(reset, ff.reset)
  (ff.q, ff.nq)
}

def clock(freq: Int)(using env: BuilderEnv): Port = newSpec {
  val clock = new Clock(freq)
  env.add(clock)
  clock.out
}

def posEdge(in: Port)(using env: BuilderEnv): Port = newSpec {
  val posEdge = new PosEdge
  env.add(posEdge)
  env.wire(in, posEdge.in)
  posEdge.out
}

def switch(in: Port, enable: Port)(using env: BuilderEnv): Port = newSpec {
  val switch = new Switch
  env.add(switch)
  env.wire(in, switch.in)
  env.wire(enable, switch.enable)
  switch.out
}
