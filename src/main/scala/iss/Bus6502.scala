package iss

/** The per-cycle bus interface of the 6502 core.
  *
  * Every [[Cpu6502.step]] performs exactly one bus cycle: either a read or a write. Addresses are 16-bit and values
  * 8-bit; implementations must mask their inputs accordingly.
  */
trait Bus6502 {

  /** Reads the byte at `addr` (16-bit). */
  def read(addr: Int): Int

  /** Writes the byte `value` (8-bit) at `addr` (16-bit). */
  def write(addr: Int, value: Int): Unit
}
