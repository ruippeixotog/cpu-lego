package component.ic

import component.BuilderAPI._
import component._
import core._

case class T74181Out(f: Bus, aEqB: Port, g: Port, cn4: Port, p: Port)

// https://cdn.datasheetspdf.com/pdf-down/7/4/1/74181_Philips.pdf
// https://www.righto.com/2017/03/inside-vintage-74181-alu-chip-how-it.html
def t74181(s: Bus, a: Bus, b: Bus, cn: Port, m: Port): Spec[T74181Out] = newSpec {
  val notM = not(m)
  val ps = a.zip(b).map { (a, b) => norM(a, and(b, s(0)), and(not(b), s(1))) }
  val gs = a.zip(b).map { (a, b) => nor(andM(not(b), a, s(2)), andM(a, b, s(3))) }

  val cs = (0 to 2)
    .scanLeft(Vector(cn)) { case (ands, i) => ands.map(and(_, gs(i))) :+ ps(i) }
    .map { ands => not(and(notM, orM(ands*))) }

  val f = (0 to 3).toVector.map { i => xor(cs(i), and(not(ps(i)), gs(i))) }

  val aEqB = f.reduce(and)
  val g = not(gs.reduce(and))
  val notP = (0 to 3).map { i => and(ps(i), gs.slice(i, 4).reduce(and)) }.reduce(or)
  val cn4 = or((cn +: gs).reduce(and), notP)

  T74181Out(f, aEqB, g, cn4, not(notP))
}
