package util

import simulator.{Index, Sim}
import util.Implicits._

case class Formatter(sim: Sim, index: Index)(
    renderer: PartialFunction[(String, Vector[Option[Boolean]]), Any] = (spec, _) => {}
) {
  final val patt = raw"(?s)([^%]*)%([0-9a-z]+)\{([^}]+)\}(.*)".r

  def print(fmt: String): Unit =
    println(format(fmt))

  def format(fmt: String): String =
    format(fmt, Vector())

  private def format(fmt: String, last: Vector[Option[Boolean]]): String = fmt match {
    case patt(head, spec, "*", tail) => head + render(spec, last) + format(tail, last)
    case patt(head, spec, path, tail) => head + render(spec, getBus(path)) + format(tail, getBus(path))
    case _ => fmt
  }

  def render(spec: String, v: Vector[Option[Boolean]]): String = spec match {
    case "b" => v.map(_.fold('x')(_.toInt)).mkString
    case "u" => v.sequence.fold("x")(_.toInt.toString)
    case "i" => v.sequence.fold("x")(_.toSignedInt.toString)
    case _ =>
      renderer
        .lift(spec, v)
        .getOrElse(throw new Exception(s"Invalid specifier: ${spec}"))
        .toString
  }

  private def getBus(path: String): Vector[Option[Boolean]] =
    index.buses.get(path).map(sim.get(_)).getOrElse(Vector(sim.get(index.ports(path))))
}
