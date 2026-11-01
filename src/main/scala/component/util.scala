package component

import component.BuilderAPI.*
import core.*
import util.halfPeriodForHz

def multi(f: (Port, Port) => Port)(ports: Port*): Port =
  ports.reduce(f)

def buffered[N <: Int](enable: Port)(xs: Bus[N]): Spec[Bus[N]] = newSpec {
  xs.map(switch(_, enable))
}

/** A clock signal running at `hz` hertz: `clock(halfPeriodForHz(hz), initialLevel)`. See [[util.halfPeriodForHz]] for
  * how the half-period is derived and how rounding affects the achieved frequency.
  */
def clockHz(hz: Double, initialLevel: Boolean = true): Spec[Port] =
  clock(halfPeriodForHz(hz), initialLevel)
