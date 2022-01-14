package computer.sap1

import util.Implicits._

enum Instr(val repr: Seq[Boolean]) {
  case LDA(addr: Int) extends Instr(0.toBoolVec(4) ++ addr.toBoolVec(4))
  case ADD(addr: Int) extends Instr(1.toBoolVec(4) ++ addr.toBoolVec(4))
  case SUB(addr: Int) extends Instr(2.toBoolVec(4) ++ addr.toBoolVec(4))
  case OUT extends Instr(14.toBoolVec(4) ++ 0.toBoolVec(4))
  case HLT extends Instr(15.toBoolVec(4) ++ 0.toBoolVec(4))
}

object Instr {
  def apply(instr: Seq[Boolean]): Option[Instr] = instr match {
    case Seq(false, false, false, false, rest: _*) => Some(LDA(rest.toInt))
    case Seq(true, false, false, false, rest: _*) => Some(ADD(rest.toInt))
    case Seq(true, true, false, false, rest: _*) => Some(SUB(rest.toInt))
    case Seq(false, true, true, true, _*) => Some(OUT)
    case Seq(true, true, true, true, _*) => Some(HLT)
    case _ => None
  }
}
