package iss

/** The per-cycle bus interface of the 6502 core.
  *
  * Every [[Cpu6502.step]] performs exactly one bus cycle: either a read or a write. Addresses are 16-bit and values
  * 8-bit; implementations must mask their inputs accordingly. Driving the core through this trait (rather than raw
  * pins) is what lets the same core later be driven by pin-level signals.
  */
trait Bus6502 {

  /** Reads the byte at `addr` (16-bit). */
  def read(addr: Int): Int

  /** Writes the byte `value` (8-bit) at `addr` (16-bit). */
  def write(addr: Int, value: Int): Unit
}
