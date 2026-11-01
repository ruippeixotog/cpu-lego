package core

import scala.annotation.implicitNotFound
import scala.compiletime.ops.any.IsConst
import scala.compiletime.ops.boolean.&&
import scala.compiletime.ops.int.*

class Port

sealed trait LogicLevel extends Port
case object High extends LogicLevel
case object Low extends LogicLevel

/** Raw, unsized ports: what the netlist, the simulators and importers work on. */
type Ports = Vector[Port]

/** A bus of `N` ports, with `bus(0)` the LSB.
  *
  * `N` is a literal for fixed-width hardware (`Bus[8]`), a type parameter for width-generic components, or `n.type`
  * when the width is a stable runtime value.
  */
final class Bus[N <: Int] private (val ports: Ports) {
  def length: Int = ports.length
  def apply(i: Int): Port = ports(i)
  def isEmpty: Boolean = ports.isEmpty

  /** A bit-wise transformation: same width by construction. */
  def map(f: Port => Port): Bus[N] = new Bus(ports.map(f))

  /** Combines two equal-width buses bit by bit. */
  def zipMap(that: Bus[N])(f: (Port, Port) => Port): Bus[N] = {
    assert(length == that.length, s"Buses must have the same width: $length != ${that.length}")
    new Bus(ports.lazyZip(that.ports).map(f))
  }

  /** Concatenation: `this` is the low part of the result. */
  def ++[M <: Int](that: Bus[M]): Bus[N + M] = new Bus(ports ++ that.ports)

  def take(k: Int)(using Width.Le[k.type, N]): Bus[k.type] = {
    assert(k <= length, s"Cannot take $k bits of a $length-bit bus")
    new Bus(ports.take(k))
  }

  def drop(k: Int)(using Width.Le[k.type, N]): Bus[N - k.type] = {
    assert(k <= length, s"Cannot drop $k bits of a $length-bit bus")
    new Bus(ports.drop(k))
  }

  def splitAt(k: Int)(using Width.Le[k.type, N]): (Bus[k.type], Bus[N - k.type]) = (take(k), drop(k))

  /** The bits as a tuple, for destructuring: `val (lo, hi) = bus.toTuple`. */
  def toTuple: Bus.Fill[N] = Tuple.fromArray(ports.toArray[Object]).asInstanceOf[Bus.Fill[N]]

  override def equals(that: Any): Boolean = that match {
    case b: Bus[?] => ports == b.ports
    case _ => false
  }
  override def hashCode: Int = ports.hashCode
  override def toString: String = ports.mkString("Bus(", ", ", ")")
}

object Bus {
  type Fill[N <: Int] <: Tuple = N match {
    case 0 => EmptyTuple
    case S[n] => Port *: Fill[n]
  }

  /** Every `Vector` operation that does not preserve a known width stays available, yielding plain vectors. */
  given toPorts[N <: Int]: Conversion[Bus[N], Ports] = _.ports

  def apply(p: Port): Bus[1] = new Bus(Vector(p))
  def apply(p1: Port, p2: Port): Bus[2] = new Bus(Vector(p1, p2))
  def apply(p1: Port, p2: Port, p3: Port): Bus[3] = new Bus(Vector(p1, p2, p3))
  def apply(p1: Port, p2: Port, p3: Port, p4: Port): Bus[4] = new Bus(Vector(p1, p2, p3, p4))

  /** Runtime-checked entry point from unsized ports to a width known by value. */
  def of(width: Int)(ports: Ports): Bus[width.type] = {
    assert(ports.length == width, s"Expected a $width-bit bus, got ${ports.length} ports")
    new Bus(ports)
  }

  /** Runtime-checked entry point for an operation known to preserve the width of `like`. */
  def sameAs[N <: Int](like: Bus[N])(ports: Ports): Bus[N] = {
    assert(ports.length == like.length, s"Expected a ${like.length}-bit bus, got ${ports.length} ports")
    new Bus(ports)
  }

  /** Escape hatch for widths the type system cannot compute. The caller asserts `ports.length == N`. */
  def assume[N <: Int](ports: Ports): Bus[N] = new Bus(ports)

  /** A bus with no static width: width constraints on it are checked at runtime. */
  def dynamic(ports: Ports): Bus[Int] = new Bus(ports)
}

/** Width constraints, proven by the compiler when both sides are constants and deferred to the component's runtime
  * asserts otherwise.
  */
object Width {
  @implicitNotFound("Bus widths do not match: ${A} is not equal to ${B}")
  sealed trait Eq[A <: Int, B <: Int]

  @implicitNotFound("Bus width out of range: ${A} is greater than ${B}")
  sealed trait Le[A <: Int, B <: Int]

  private type BothConst[A, B] = IsConst[A] && IsConst[B]

  private object Proof extends Eq[Int, Int] with Le[Int, Int]

  sealed trait EqLow {
    given deferred[A <: Int, B <: Int](using BothConst[A, B] =:= false): Eq[A, B] = Proof.asInstanceOf[Eq[A, B]]
  }
  sealed trait EqDyn extends EqLow {
    given dynamic[B <: Int]: Eq[Int, B] = Proof.asInstanceOf[Eq[Int, B]]
  }
  object Eq extends EqDyn {
    given proven[A <: Int, B <: Int](using A =:= B): Eq[A, B] = Proof.asInstanceOf[Eq[A, B]]
  }

  sealed trait LeLow {
    given deferred[A <: Int, B <: Int](using BothConst[A, B] =:= false): Le[A, B] = Proof.asInstanceOf[Le[A, B]]
  }
  sealed trait LeDyn extends LeLow {
    given dynamicL[B <: Int]: Le[Int, B] = Proof.asInstanceOf[Le[Int, B]]
    given dynamicR[A <: Int]: Le[A, Int] = Proof.asInstanceOf[Le[A, Int]]
  }
  object Le extends LeDyn {
    given proven[A <: Int, B <: Int](using (A <= B) =:= true): Le[A, B] = Proof.asInstanceOf[Le[A, B]]
  }
}

sealed trait Component
sealed trait BaseComponent extends Component

case class NAND(in1: Port, in2: Port, out: Port) extends BaseComponent
case class Clock(freq: Int, out: Port) extends BaseComponent
case class Switch(in: Port, out: Port, enable: Port) extends BaseComponent

enum Direction {
  case Input, Output, Inout
}

case class CompositeComponent(
    name: String,
    components: Map[String, Component],
    wires: List[(Port, Port)],
    namedPorts: Map[String, (Option[Direction], Port | Ports)]
) extends Component
