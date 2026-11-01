package component.i8086

import component.*
import component.BuilderAPI.*
import core.*

// 8086 decimal-adjust helpers (DAA, DAS, AAA, AAS).
//
// The low-nibble test runs first and its result feeds the high-nibble test.

/** The high-nibble adjustment for [[daa]] and [[das]]: set iff CF is set, or the high nibble exceeds 9, or AF is clear
  * and the high nibble is 9 with the low nibble exceeding 9. AL > 0x9F is a high nibble above 9, and AL > 0x99 adds the
  * case of a high nibble of 9 with a low nibble above 9.
  */
private def decimalHighAdj(al: Bus[8], af: Port, cf: Port): Spec[Port] = newSpec {
  val high = al.drop(4)
  orM(cf, nibbleGt9(high), andM(not(af), eqConst(high, 9), nibbleGt9(al.take(4))))
}

/** 8086 DAA (decimal adjust AL after addition).
  *
  * Takes the AL byte after an addition, with the AF and CF produced by that addition. If the low nibble exceeds 9 or AF
  * is set, 6 is added to AL and AF is set, else AF is cleared; then, using the original AL, if it exceeds 0x9F (or 0x99
  * when AF is clear) or CF is set, 0x60 is added to AL and CF is set, else CF is cleared.
  *
  * This is the 8088 behaviour; Intel's current pseudocode uses a fixed 0x99.
  *
  * Returns `(result, cf, af)`, in that order. SF, ZF and PF for the result follow from [[flagN]], [[flagZ]] and
  * [[flagP]] and are not returned here.
  */
def daa(al: Bus[8], af: Port, cf: Port): Spec[(Bus[8], Port, Port)] = newSpec {
  val lowAdj = or(nibbleGt9(al.take(4)), af)
  val (alPlus6, _, _) = rippleAdder(al, const(8, 6), Low)
  val alLow = mux2(al, alPlus6, lowAdj)
  val highAdj = decimalHighAdj(al, af, cf)
  val (alPlus60, _, _) = rippleAdder(alLow, const(8, 0x60), Low)
  (mux2(alLow, alPlus60, highAdj), highAdj, lowAdj)
}

/** 8086 DAS (decimal adjust AL after subtraction).
  *
  * The subtraction mirror of [[daa]]: if the low nibble exceeds 9 or AF is set, 6 is subtracted from AL and AF is set,
  * else AF is cleared; then, using the original AL, if it exceeds 0x9F (or 0x99 when AF is clear) or CF is set, 0x60 is
  * subtracted from AL and CF is set, else CF is cleared.
  *
  * Returns `(result, cf, af)`, in that order.
  */
def das(al: Bus[8], af: Port, cf: Port): Spec[(Bus[8], Port, Port)] = newSpec {
  val lowAdj = or(nibbleGt9(al.take(4)), af)
  val (alMinus6, _, _) = subtractor(al, const(8, 6))
  val alLow = mux2(al, alMinus6, lowAdj)
  val highAdj = decimalHighAdj(al, af, cf)
  val (alMinus60, _, _) = subtractor(alLow, const(8, 0x60))
  (mux2(alLow, alMinus60, highAdj), highAdj, lowAdj)
}

/** 8086 AAA (ASCII adjust AL after addition).
  *
  * If the low nibble of AL exceeds 9 or AF is set, AL is increased by 6, AH is incremented, and AF and CF are set; else
  * AF and CF are cleared. In both cases the high nibble of AL is cleared. The incoming CF is not an input to AAA (the
  * instruction sets CF equal to AF), so it takes only `(al, ah, af)`.
  *
  * Returns `(al, ah, af, cf)`, in that order.
  */
def aaa(al: Bus[8], ah: Bus[8], af: Port): Spec[(Bus[8], Bus[8], Port, Port)] = newSpec {
  val adj = or(nibbleGt9(al.take(4)), af)
  val (alPlus6, _, _) = rippleAdder(al, const(8, 6), Low)
  val alAdj = mux2(al, alPlus6, adj)
  val (ahPlus1, _) = incrementer(ah, adj)
  (alAdj.take(4) ++ Bus.fill(4)(Low), ahPlus1, adj, adj)
}

/** 8086 AAS (ASCII adjust AL after subtraction).
  *
  * The subtraction mirror of [[aaa]]: if the low nibble of AL exceeds 9 or AF is set, AL is decreased by 6, AH is
  * decremented, and AF and CF are set; else AF and CF are cleared. In both cases the high nibble of AL is cleared. The
  * incoming CF is not an input to AAS.
  *
  * Returns `(al, ah, af, cf)`, in that order.
  */
def aas(al: Bus[8], ah: Bus[8], af: Port): Spec[(Bus[8], Bus[8], Port, Port)] = newSpec {
  val adj = or(nibbleGt9(al.take(4)), af)
  val (alMinus6, _, _) = subtractor(al, const(8, 6))
  val alAdj = mux2(al, alMinus6, adj)
  val (ahMinus1, _) = decrementer(ah, adj)
  (alAdj.take(4) ++ Bus.fill(4)(Low), ahMinus1, adj, adj)
}
