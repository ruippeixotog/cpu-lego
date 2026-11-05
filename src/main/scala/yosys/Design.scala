package yosys

import java.nio.file.Path

import pureconfig.*
import pureconfig.ConfigReader.Result
import pureconfig.error.CannotConvert
import pureconfig.generic.ProductHint
import pureconfig.generic.semiauto.*

case class Design(creator: String, modules: Map[String, Design.Module])

object Design {

  case class Module(
      attributes: Map[String, String],
      ports: Map[String, Port],
      cells: Map[String, Cell],
      netnames: Map[String, Net]
  )

  case class Port(direction: Direction, bits: Vector[Int])

  object Port {}

  case class Cell(
      hideName: Int,
      `type`: String,
      parameters: Map[String, String],
      attributes: Map[String, String],
      portDirections: Map[String, Direction],
      connections: Map[String, Vector[Int | String]]
  )

  // `port_directions` is absent from the JSON for cells Yosys did not derive itself
  // (e.g. `$_TBUF_` instantiated directly in Verilog rather than produced by techmap).
  private case class RawCell(
      hideName: Int,
      `type`: String,
      parameters: Map[String, String],
      attributes: Map[String, String],
      portDirections: Option[Map[String, Direction]],
      connections: Map[String, Vector[Int | String]]
  )

  case class Net(
      hideName: Int,
      bits: Vector[Int | String],
      attributes: Map[String, String]
  )

  enum Direction {
    case Input, Output, Inout

    def reverse: Direction = this match {
      case Input => Output
      case Output => Input
      case Inout => Inout
    }
  }

  private given [A] => ProductHint[A] = ProductHint(ConfigFieldMapping(CamelCase, SnakeCase))
  private given ConfigReader[String] = new ConfigReader[String] {
    override def from(cur: ConfigCursor): Result[String] =
      cur.asConfigValue.flatMap { cv =>
        if (cv.unwrapped.isInstanceOf[String])
          Right(cv.unwrapped.asInstanceOf[String])
        else
          cur.failed(CannotConvert(cv.toString, "String", "Not a string"))
      }
  }
  private given ConfigReader[Int | String] = ConfigReader[String].orElse(ConfigReader[Int])
  private given ConfigReader[Direction] = deriveEnumerationReader
  private given ConfigReader[Module] = deriveReader
  private given ConfigReader[Port] = deriveReader
  private given ConfigReader[RawCell] = deriveReader
  private given ConfigReader[Cell] =
    ConfigReader[RawCell].map(r =>
      Cell(r.hideName, r.`type`, r.parameters, r.attributes, r.portDirections.getOrElse(Map.empty), r.connections)
    )
  private given ConfigReader[Net] = deriveReader
  given ConfigReader[Design] = deriveReader

  def fromJsonFile(file: Path): Design =
    ConfigSource.file(file).loadOrThrow[Design]
}
