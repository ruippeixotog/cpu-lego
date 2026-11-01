package component

import scala.compiletime.ops.int.*

import component.BuilderAPI.*
import core.*

/** An `ins.length` to 2^`ins.length` decoder.
  */
def decoder[N <: Int](ins: Bus[N], enable: Port): Spec[Bus[1 << N]] = newSpec {
  def aux(ins0: Ports, enable0: Port): Ports = ins0 match {
    case rest :+ in => aux(rest, and(enable0, not(in))) ++ aux(rest, and(enable0, in))
    case _ => Vector(enable0)
  }
  Bus.assume(aux(ins.ports, enable))
}

/** An `ins.length` to 1 multiplexer. `ins.length` must be equal to 2^`sel.length`.
  */
def mux[N <: Int, S <: Int](ins: Bus[N], sel: Bus[S])(using Width.Eq[N, 1 << S]): Spec[Port] = newSpec {
  assert(ins.length == 1 << sel.length, "Input and address bus sizes do not match")
  ins.ports.zip(decoder(sel, High).ports).map(and).reduce(or)
}

/** An `ins.length` to `width` multiplexer. `ins.length` must be equal to 2^`sel.length` * width.
  */
def muxN[N <: Int, S <: Int](ins: Bus[N], sel: Bus[S], width: Int)(using
    Width.Eq[N, (1 << S) * width.type]
): Spec[Bus[width.type]] = newSpec {
  assert(ins.length == (1 << sel.length) * width, "Input and address bus sizes do not match for given width")

  val words = ins.ports.grouped(width).toVector
  Bus.of(width)((0 until width).map { i => mux(Bus.dynamic(words.map(_(i))), Bus.dynamic(sel.ports)) }.toVector)
}
