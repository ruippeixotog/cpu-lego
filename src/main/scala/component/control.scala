package component

import component.BuilderAPI._
import core._

def decoder(ins: Bus, enable: Port): Spec[Bus] = newSpec {
  def aux(ins0: Bus, enable0: Port): Bus = ins0 match {
    case rest :+ in => aux(rest, and(enable0, not(in))) ++ aux(rest, and(enable0, in))
    case _ => Vector(enable0)
  }
  aux(ins, enable)
}
