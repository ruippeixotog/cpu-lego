package component

import component.BuilderAPI._
import core._

def nand(_in1: Port, _in2: Port): Spec[Port] = newSpec {
  val in1, in2, out = newPort()
  summon[BuilderEnv].add("impl", NAND(in1, in2, out))
  _in1 ~> in1
  _in2 ~> in2
  out
}

def flipflop(_set: Port, _reset: Port): Spec[(Port, Port)] = newSpec {
  val set, reset, q, nq = newPort()
  summon[BuilderEnv].add("impl", FlipFlop(set, reset, q, nq))
  _set ~> set
  _reset ~> reset
  (q, nq)
}

def clock(freq: Int): Spec[Port] = newSpec {
  val out = newPort()
  summon[BuilderEnv].add("impl", Clock(freq, out))
  out
}

def posEdge(_in: Port): Spec[Port] = newSpec {
  val in, out = newPort()
  summon[BuilderEnv].add("impl", PosEdge(in, out))
  _in ~> in
  out
}

def switch(_in: Port, _enable: Port): Spec[Port] = newSpec {
  val in, out, enable = newPort()
  summon[BuilderEnv].add("impl", Switch(in, out, enable))
  _in ~> in
  _enable ~> enable
  out
}
