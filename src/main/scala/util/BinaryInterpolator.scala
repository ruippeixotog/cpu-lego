package util

import util.Implicits.*

class BinaryInterpolator(sc: StringContext) {

  def apply(args: Any*): Vector[Boolean] =
    vec(sc.parts.head) ++ args
      .zip(sc.parts.tail)
      .map {
        case (arg: Boolean, rest) => arg +: vec(rest)
        case (arg: Int, s"{$n}$rest") => arg.toBoolVec(n.toInt) ++: vec(rest)
        case (arg: Vector[Boolean] @unchecked, rest) => arg ++ vec(rest)
        case (arg, _) => sys.error("Illegal argument type: " + arg.getClass)
      }
      .flatten

  def unapplySeq(arg: Vector[Boolean]): Option[Seq[Int]] = {
    val literals = sc.parts.iterator

    val (argPart0, rest) = arg.splitAt(sc.parts.head.length)
    if (vec(sc.parts.head) != argPart0) None
    else {
      sc.parts.tail
        .foldLeft[(Option[(Seq[Int], Vector[Boolean])])]((Some((Nil, rest)))) {
          case (None, _) => None
          case (Some(matches, rest), s"{$n}$part") =>
            val (argMatched, rest1) = rest.splitAt(n.toInt)
            val (argPart, rest2) = rest1.splitAt(part.length)

            if (vec(part) != argPart) None
            else Some(matches :+ argMatched.toInt, rest2)

          case (Some(matches, rest), part) =>
            sys.error("Missing pattern match length {n}")
        }
        .flatMap {
          case (arg, Vector()) => Some(arg)
          case _ => None
        }
    }
  }

  def vec(str: String): Vector[Boolean] = str.toVector.map {
    case '1' => true
    case '0' => false
    case ch => sys.error("Illegal character in binary string: " + ch)
  }
}
