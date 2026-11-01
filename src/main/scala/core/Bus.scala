package core

import scala.compiletime.ops.int.*

/** A bus of `N` ports, with `bus(0)` the LSB.
  *
  * `N` is a literal for fixed-width hardware (`Bus[8]`), a type parameter for width-generic components, or `n.type`
  * when the width is a stable runtime value.
  *
  * Operations that know the width of their result are members and keep it in the type; everything else is available
  * through the conversion to [[Ports]].
  */
final class Bus[N <: Int] private (val ports: Ports) {
  def length: Int = ports.length
  def apply(i: Int): Port = ports(i)
  def isEmpty: Boolean = ports.isEmpty

  def map(f: Port => Port): Bus[N] = new Bus(ports.map(f))

  def mapWithIndex(f: (Port, Int) => Port): Bus[N] = new Bus(ports.zipWithIndex.map(f.tupled))

  /** Maps the bits from the LSB up, threading a state through them. Returns the final state. */
  def mapAccum[S](init: S)(f: (S, Port) => (Port, S)): (Bus[N], S) = {
    val (out, end) = ports.foldLeft((Vector.empty[Port], init)) { case ((out, s), p) =>
      val (q, next) = f(s, p)
      (out :+ q, next)
    }
    (new Bus(out), end)
  }

  /** Maps the bits from the LSB up, feeding each output into the next bit. `init` feeds bit 0. */
  def chain(init: Port)(f: (Port, Port) => Port): Bus[N] =
    mapAccum(init) { (prev, p) =>
      val q = f(prev, p)
      (q, q)
    }._1

  /** `scanRight` seeded with the MSB, so that bit `i` of the result combines bit `i` and every bit above it. */
  def scanRight1(f: (Port, Port) => Port): Bus[N] = new Bus(ports.init.scanRight(ports.last)(f))

  def zip(that: Bus[N]): Vector[(Port, Port)] = {
    assert(length == that.length, s"Buses must have the same width: $length != ${that.length}")
    ports.zip(that.ports)
  }

  /** Combines two equal-width buses bit by bit. */
  def zipMap(that: Bus[N])(f: (Port, Port) => Port): Bus[N] = new Bus(zip(that).map(f.tupled))

  /** [[mapAccum]] over two equal-width buses. */
  def zipMapAccum[S](that: Bus[N])(init: S)(f: (S, Port, Port) => (Port, S)): (Bus[N], S) = {
    val (out, end) = zip(that).foldLeft((Vector.empty[Port], init)) { case ((out, s), (x, y)) =>
      val (q, next) = f(s, x, y)
      (out :+ q, next)
    }
    (new Bus(out), end)
  }

  /** Concatenation: `this` is the low part of the result. */
  def ++[M <: Int](that: Bus[M]): Bus[N + M] = new Bus(ports ++ that.ports)

  /** Appends `p` as the new MSB. */
  def :+(p: Port): Bus[N + 1] = new Bus(ports :+ p)

  /** Prepends `p` as the new LSB. */
  def +:(p: Port): Bus[1 + N] = new Bus(p +: ports)

  def take(k: Int)(using Width.Le[k.type, N]): Bus[k.type] = {
    assert(k <= length, s"Cannot take $k bits of a $length-bit bus")
    new Bus(ports.take(k))
  }

  def drop(k: Int)(using Width.Le[k.type, N]): Bus[N - k.type] = {
    assert(k <= length, s"Cannot drop $k bits of a $length-bit bus")
    new Bus(ports.drop(k))
  }

  def splitAt(k: Int)(using Width.Le[k.type, N]): (Bus[k.type], Bus[N - k.type]) = (take(k), drop(k))

  /** Splits the bus into consecutive `width`-bit words, the first one holding the LSB. */
  def grouped(width: Int): Vector[Bus[width.type]] = {
    assert(width > 0 && length % width == 0, s"Cannot split a $length-bit bus into $width-bit words")
    ports.grouped(width).map(Bus.of(width)).toVector
  }

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

  /** Every `Vector` operation that does not keep track of a width stays available, yielding plain vectors. */
  given toPorts[N <: Int]: Conversion[Bus[N], Ports] = _.ports

  /** A literal bus: `Bus(a)`, `Bus(a, b)`, `Bus(a, b, c, ...)`. */
  def apply(p: Port): Bus[1] = new Bus(Vector(p))
  def apply[T <: NonEmptyTuple](ports: T)(using Tuple.Union[T] <:< Port): Bus[Tuple.Size[T]] =
    new Bus(ports.toList.asInstanceOf[List[Port]].toVector)

  def fill(width: Int)(p: => Port): Bus[width.type] = new Bus(Vector.fill(width)(p))

  def tabulate(width: Int)(f: Int => Port): Bus[width.type] = new Bus(Vector.tabulate(width)(f))

  /** Runtime-checked entry point from raw ports, for widths that only exist at the boundary of the DSL. */
  def of(width: Int)(ports: Ports): Bus[width.type] = sized[width.type](width)(ports)

  /** Runtime-checked entry point for a width the compiler cannot derive: `width` must be the value of `N`. */
  def sized[N <: Int](width: Int)(ports: Ports): Bus[N] = {
    assert(ports.length == width, s"Expected a $width-bit bus, got ${ports.length} ports")
    new Bus(ports)
  }

  /** A bus with no static width: width constraints on it are checked at runtime. */
  def dynamic(ports: Ports): Bus[Int] = new Bus(ports)
}
