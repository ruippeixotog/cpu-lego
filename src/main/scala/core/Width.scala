package core

import scala.annotation.implicitNotFound
import scala.compiletime.ops.any.IsConst
import scala.compiletime.ops.boolean.&&
import scala.compiletime.ops.int.*

/** Type-level widths: width arithmetic, and constraints that are proven by the compiler when both sides are constants
  * and deferred to the runtime asserts of the component otherwise.
  */
object Width {

  /** `ceil(log2(N))`: the width of a binary index into `N` items. */
  type CLog2[N <: Int] = 32 - NumberOfLeadingZeros[N - 1]

  /** The value of [[CLog2]]. */
  def clog2(n: Int): Int = 32 - Integer.numberOfLeadingZeros(n - 1)

  @implicitNotFound("Bus widths do not match: ${A} is not equal to ${B}")
  sealed trait Eq[A <: Int, B <: Int] {

    /** Views a bus of the proven-equal width `B` as a `Bus[A]`. */
    def apply(bus: Bus[B]): Bus[A] = Bus.sized[A](bus.length)(bus.ports)
  }

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
