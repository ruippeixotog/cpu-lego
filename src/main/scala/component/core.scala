package component

import component.BuilderAPI.*
import core.*

/** A NAND gate (https://en.wikipedia.org/wiki/NAND_gate).
  */
def nand(in1: Port, in2: Port): Spec[Port] = newSpec {
  val _in1, _in2, out = new Port()
  summon[BuilderEnv].add("impl", NAND(_in1, _in2, out))
  in1 ~> _in1
  in2 ~> _in2
  out
}

/** A clock signal (https://en.wikipedia.org/wiki/Clock_signal).
  */
def clock(freq: Int): Spec[Port] = newSpec {
  val out = new Port()
  summon[BuilderEnv].add("impl", Clock(freq, out))
  out
}

/** A switch enabling three-state logic (https://en.wikipedia.org/wiki/Three-state_logic).
  */
def switch(in: Port, enable: Port): Spec[Port] = newSpec {
  val _in, _enable, out = new Port()
  summon[BuilderEnv].add("impl", Switch(_in, out, _enable))
  in ~> _in
  enable ~> _enable
  out
}
