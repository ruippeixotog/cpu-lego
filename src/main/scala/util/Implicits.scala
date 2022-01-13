package util

import scala.annotation.targetName

import core._

object Implicits {

  extension (self: LogicLevel) {
    def toBool: Boolean = self match {
      case High => true
      case Low => false
    }

    def toInt: Int = self match {
      case High => 1
      case Low => 0
    }
  }

  extension (self: Option[LogicLevel]) {
    def toPort: Port = self.getOrElse(new Port)
  }

  extension (self: Seq[LogicLevel]) {
    def toInt: Int = util.Implicits.toInt(self.map(_.toBool))
  }

  extension (self: Boolean) {
    def toInt: Int = if (self) 1 else 0
  }

  extension (self: Seq[Boolean]) {
    @targetName("seqBoolToInt")
    def toInt: Int = self.zipWithIndex.filter(_._1).foldLeft(0)((acc, i) => acc | (1 << i._2))

    def toSignedInt: Int = if (self.last) -self.init.toInt - 1 else self.init.toInt
  }

  extension (self: Seq[Option[Boolean]]) {
    def sequence: Option[Seq[Boolean]] =
      self.foldRight[Option[List[Boolean]]](Some(Nil)) { case (x, acc) => x.zip(acc).map(_ :: _) }

    def render: String = render("%b")

    def render(fmt: String): String =
      fmt
        .replaceAll("%s", self.toString)
        .replaceAll("%b", self.map(_.fold('x')(_.toInt)).mkString)
        .replaceAll("%u", self.sequence.fold("x")(_.toInt.toString))
        .replaceAll("%i", self.sequence.fold("x")(_.toSignedInt.toString))
  }

  extension (self: Int) {
    def toBoolVec(n: Int): Seq[Boolean] = (0 until n).map(i => (self & (1 << i)) != 0)
  }
}
