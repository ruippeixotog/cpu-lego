package component.ic

import component.*
import component.BuilderAPI.*
import core.*

case class T74181Out(f: Bus[4], aEqB: Port, g: Port, cn4: Port, p: Port)

// https://cdn.datasheetspdf.com/pdf-down/7/4/1/74181_Philips.pdf
// https://www.righto.com/2017/03/inside-vintage-74181-alu-chip-how-it.html
def t74181(s: Bus[4], a: Bus[4], b: Bus[4], cn: Port, m: Port): Spec[T74181Out] = newSpec {
  val notM = not(m)
  val ps = a.ports.zip(b.ports).map { (a, b) => norM(a, and(b, s(0)), and(not(b), s(1))) }
  val gs = a.ports.zip(b.ports).map { (a, b) => nor(andM(not(b), a, s(2)), andM(a, b, s(3))) }

  val cs = (0 to 2)
    .scanLeft(Vector(cn)) { case (ands, i) => ands.map(and(_, gs(i))) :+ ps(i) }
    .map { ands => not(and(notM, orM(ands*))) }

  val f = (0 to 3).toVector.map { i => xor(cs(i), and(not(ps(i)), gs(i))) }

  val aEqB = f.reduce(and)
  val g = not(gs.reduce(and))
  val notP = (0 to 3).map { i => and(ps(i), gs.slice(i, 4).reduce(and)) }.reduce(or)
  val cn4 = or((cn +: gs).reduce(and), notP)

  T74181Out(Bus.of(4)(f), aEqB, g, cn4, not(notP))
}
