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

  /** `0 <= A < B`. */
  @implicitNotFound("Bit ${A} is not provably within a ${B}-bit bus: use a Bus.Index for it, or `get` if computed")
  sealed trait Lt[A <: Int, B <: Int]

  private type InRange[A <: Int, B <: Int] = (0 <= A) && (A < B)

  private object Proof extends Eq[Int, Int] with Le[Int, Int] with Lt[Int, Int]

  sealed trait EqDeferredB {
    given deferredB[A <: Int, B <: Int](using IsConst[B] =:= false): Eq[A, B] = Proof.asInstanceOf[Eq[A, B]]
  }
  sealed trait EqDeferredA extends EqDeferredB {
    given deferredA[A <: Int, B <: Int](using IsConst[A] =:= false): Eq[A, B] = Proof.asInstanceOf[Eq[A, B]]
  }
  object Eq extends EqDeferredA {
    given proven[A <: Int, B <: Int](using A =:= B): Eq[A, B] = Proof.asInstanceOf[Eq[A, B]]
  }

  sealed trait LeDeferredB {
    given deferredB[A <: Int, B <: Int](using IsConst[B] =:= false): Le[A, B] = Proof.asInstanceOf[Le[A, B]]
  }
  sealed trait LeDeferredA extends LeDeferredB {
    given deferredA[A <: Int, B <: Int](using IsConst[A] =:= false): Le[A, B] = Proof.asInstanceOf[Le[A, B]]
  }
  object Le extends LeDeferredA {
    given proven[A <: Int, B <: Int](using (A <= B) =:= true): Le[A, B] = Proof.asInstanceOf[Le[A, B]]
  }

  sealed trait LtDeferred {
    given deferred[A <: Int, B <: Int](using IsConst[B] =:= false): Lt[A, B] = Proof.asInstanceOf[Lt[A, B]]
  }
  object Lt extends LtDeferred {
    given proven[A <: Int, B <: Int](using InRange[A, B] =:= true): Lt[A, B] = Proof.asInstanceOf[Lt[A, B]]
  }
}
