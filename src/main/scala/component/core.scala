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
  *
  * Toggles every `halfPeriod` ticks (1 tick = 1 ns), starting at `initialLevel` at t=0.
  */
def clock(halfPeriod: Int, initialLevel: Boolean = true): Spec[Port] = newSpec {
  val out = new Port()
  summon[BuilderEnv].add("impl", Clock(halfPeriod, out, initialLevel))
  out
}

/** A clock signal running at `hz` hertz: `clock(Clock.halfPeriodForHz(hz), initialLevel)`. See
  * [[core.Clock.halfPeriodForHz]] for how the half-period is derived and how rounding affects the achieved frequency.
  */
def clockHz(hz: Double, initialLevel: Boolean = true): Spec[Port] =
  clock(Clock.halfPeriodForHz(hz), initialLevel)

/** A switch enabling three-state logic (https://en.wikipedia.org/wiki/Three-state_logic).
  */
def switch(in: Port, enable: Port): Spec[Port] = newSpec {
  val _in, _enable, out = new Port()
  summon[BuilderEnv].add("impl", Switch(_in, out, _enable))
  in ~> _in
  enable ~> _enable
  out
}
