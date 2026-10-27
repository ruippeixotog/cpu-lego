package component

import component.BuilderAPI.*
import core.*

def multi(f: (Port, Port) => Port)(ports: Port*): Port =
  ports.reduce(f)

def buffered(enable: Port)(xs: Bus): Spec[Bus] = newSpec {
  xs.map(switch(_, enable))
}

/** Half-period in ticks for a clock running at `hz` hertz: `1e9 / (2 * hz)`, rounded to the nearest tick and clamped to
  * at least 1.
  *
  * The conversion applies the simulation time base, 1 tick = 1 nanosecond: at that base, a half-period lasts
  * `1e9 / (2 * hz)` nanoseconds. Rounding means the achieved frequency, `1e9 / (2 * halfPeriod)` Hz, can differ from
  * `hz` by up to half a tick per half-period — negligible for slow clocks, worth double-checking near the 500 MHz
  * ceiling.
  */
def halfPeriodForHz(hz: Double): Int = {
  require(hz > 0, "hz must be positive")
  math.max(1, math.round(1e9 / (2 * hz)).toInt)
}

/** A clock signal running at `hz` hertz: `clock(halfPeriodForHz(hz), initialLevel)`. See [[halfPeriodForHz]] for how
  * the half-period is derived and how rounding affects the achieved frequency.
  */
def clockHz(hz: Double, initialLevel: Boolean = true): Spec[Port] =
  clock(halfPeriodForHz(hz), initialLevel)
