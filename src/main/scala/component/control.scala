package component

import component.BuilderAPI._
import core._

/** An `ins.length` to 2^`ins.length` decoder.
  */
def decoder(ins: Bus, enable: Port): Spec[Bus] = newSpec {
  def aux(ins0: Bus, enable0: Port): Bus = ins0 match {
    case rest :+ in => aux(rest, and(enable0, not(in))) ++ aux(rest, and(enable0, in))
    case _ => Vector(enable0)
  }
  aux(ins, enable)
}

/** An `ins.length` to 1 multiplexer. `ins.length` must be equal to 2^`sel.length`.
  */
def mux(ins: Bus, sel: Bus): Spec[Port] = newSpec {
  assert(ins.length == 1 << sel.length, "Input and address bus sizes do not match")
  ins.zip(decoder(sel, High)).map(and).reduce(or)
}
