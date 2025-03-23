package computer.sap1

import util.Implicits.*

enum Instr(val repr: Vector[Boolean]) {
  case LDA(addr: Int) extends Instr(b"0000$addr{4}")
  case ADD(addr: Int) extends Instr(b"1000$addr{4}")
  case SUB(addr: Int) extends Instr(b"0100$addr{4}")
  case OUT extends Instr(b"01110000")
  case HLT extends Instr(b"11110000")
}

object Instr {
  def apply(instr: Seq[Boolean]): Option[Instr] = instr.toVector match {
    case b"0000$addr{4}" => Some(LDA(addr))
    case b"1000$addr{4}" => Some(ADD(addr))
    case b"0100$addr{4}" => Some(SUB(addr))
    case b"01110000" => Some(OUT)
    case b"11110000" => Some(HLT)
    case _ => None
  }
}
