package component

import component.BuilderAPI._
import core._

def nand(in1: Port, in2: Port): Spec[Port] = newSpec {
  val env = summon[BuilderEnv]
  val nand = new NAND
  env.add("impl", nand)
  env.wire(in1, nand.in1)
  env.wire(in2, nand.in2)
  nand.out
}

def flipflop(set: Port, reset: Port): Spec[(Port, Port)] = newSpec {
  val env = summon[BuilderEnv]
  val ff = new FlipFlop
  env.add("impl", ff)
  env.wire(set, ff.set)
  env.wire(reset, ff.reset)
  (ff.q, ff.nq)
}

def clock(freq: Int): Spec[Port] = newSpec {
  val env = summon[BuilderEnv]
  val clock = new Clock(freq)
  env.add("impl", clock)
  clock.out
}

def posEdge(in: Port): Spec[Port] = newSpec {
  val env = summon[BuilderEnv]
  val posEdge = new PosEdge
  env.add("impl", posEdge)
  env.wire(in, posEdge.in)
  posEdge.out
}

def switch(in: Port, enable: Port): Spec[Port] = newSpec {
  val env = summon[BuilderEnv]
  val switch = new Switch
  env.add("impl", switch)
  env.wire(in, switch.in)
  env.wire(enable, switch.enable)
  switch.out
}
