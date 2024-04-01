package component

import component.BuilderAPI._
import core._

def posEdge(in: Port): Spec[Port] = newSpec {
  and(in, not(in))
}

def negEdge(in: Port): Spec[Port] = newSpec {
  and(not(in), not(not(in)))
}

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

/** An `ins.length` to `width` multiplexer. `ins.length` must be equal to 2^`sel.length` * width.
  */
def muxN(ins: Bus, sel: Bus, width: Int): Spec[Bus] = newSpec {
  assert(ins.length == (1 << sel.length) * width, "Input and address bus sizes do not match for given width")

  val words = ins.grouped(width).toVector
  (0 until width).map { i => mux(words.map(_(i)), sel) }.toVector
}
