package util

import simulator.{Index, Sim}
import util.Implicits._

case class Formatter(sim: Sim, index: Index)(
    renderer: PartialFunction[(String, Vector[Option[Boolean]]), Any] = (spec, _) => {}
) {
  final val patt = raw"(?s)([^%]*)%([0-9a-z]+)\{([^}\[]+)(?:\[(\d+),(\d+)\])?\}(.*)".r

  def print(fmt: String): Unit =
    println(format(fmt))

  def format(fmt: String): String =
    format(fmt, Vector())

  private def format(fmt: String, last: Vector[Option[Boolean]]): String = fmt match {
    case patt(head, spec, path, from, until, tail) =>
      val bus = if (path == "*") last else getBus(path)
      val bus1 = if (from == null) bus else bus.slice(from.toInt, until.toInt)
      head + render(spec, bus1) + format(tail, bus)
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
