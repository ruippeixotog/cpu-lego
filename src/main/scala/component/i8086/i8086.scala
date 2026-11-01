package component.i8086

import component.*
import component.BuilderAPI.*
import core.*

// 8086 decimal-adjust helpers (DAA, DAS, AAA, AAS).
//
// The low-nibble test runs first and its result feeds the high-nibble test. DAA and DAS test the *original* AL against
// an AF-dependent threshold (see [[daa]]), which is the 8088 behaviour; it differs from Intel's current pseudocode
// for AF = 1 and AL in 0x9A..0x9F.

/** A nibble is greater than 9 iff its top bit is set and either of the next two bits is set (values 10..15). */
private def nibbleGt9(nibble: Bus): Spec[Port] = newSpec {
  assert(nibble.length == 4, "A nibble must be 4 bits")
  and(nibble(3), or(nibble(2), nibble(1)))
}

/** 8086 DAA (decimal adjust AL after addition).
  *
  * Takes the AL byte after an addition, with the AF and CF produced by that addition. If the low nibble exceeds 9 or AF
  * is set, 6 is added to AL and AF is set, else AF is cleared; then, using the original AL, if it exceeds 0x9F (or 0x99
  * when AF is clear) or CF is set, 0x60 is added to AL and CF is set, else CF is cleared.
  *
  * The threshold depends on AF because the low adjustment already happened when AF is set: values 0x9A..0x9F with AF
  * set do not get the 0x60, where Intel's current pseudocode — which tests the adjusted AL against a fixed 0x99 — would
  * add it. This is the 8088 behaviour, confirmed against SingleStepTests/8088 27.
  *
  * Returns `(result, cf, af)`, in that order. SF, ZF and PF for the result follow from [[flagN]], [[flagZ]] and
  * [[flagP]] and are not returned here.
  */
def daa(al: Bus, af: Port, cf: Port): Spec[(Bus, Port, Port)] = newSpec {
  assert(al.length == 8, "DAA operates on an 8-bit AL")
  val lowAdj = or(nibbleGt9(al.take(4)), af)
  val (alPlus6, _, _) = rippleAdder(al, const(8, 6), Low)
  val alLow = mux2(al, alPlus6, lowAdj)
  // The high test uses the original AL: AL > 0x99 iff AL >= 0x9A, AL > 0x9F iff AL >= 0xA0.
  val (_, below9A, _) = compare(al, const(8, 0x9a))
  val (_, belowA0, _) = compare(al, const(8, 0xa0))
  val highAdj = or(cf, or(and(af, not(belowA0)), and(not(af), not(below9A))))
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
def das(al: Bus, af: Port, cf: Port): Spec[(Bus, Port, Port)] = newSpec {
  assert(al.length == 8, "DAS operates on an 8-bit AL")
  val lowAdj = or(nibbleGt9(al.take(4)), af)
  val (alMinus6, _, _) = subtractor(al, const(8, 6))
  val alLow = mux2(al, alMinus6, lowAdj)
  // The high test uses the original AL: AL > 0x99 iff AL >= 0x9A, AL > 0x9F iff AL >= 0xA0.
  val (_, below9A, _) = compare(al, const(8, 0x9a))
  val (_, belowA0, _) = compare(al, const(8, 0xa0))
  val highAdj = or(cf, or(and(af, not(belowA0)), and(not(af), not(below9A))))
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
def aaa(al: Bus, ah: Bus, af: Port): Spec[(Bus, Bus, Port, Port)] = newSpec {
  assert(al.length == 8 && ah.length == 8, "AAA operates on 8-bit AL and AH")
  val adj = or(nibbleGt9(al.take(4)), af)
  val (alPlus6, _, _) = rippleAdder(al, const(8, 6), Low)
  val alAdj = mux2(al, alPlus6, adj)
  val (ahPlus1, _) = incrementer(ah, adj)
  (alAdj.take(4) ++ Vector.fill(4)(Low), ahPlus1, adj, adj)
}

/** 8086 AAS (ASCII adjust AL after subtraction).
  *
  * The subtraction mirror of [[aaa]]: if the low nibble of AL exceeds 9 or AF is set, AL is decreased by 6, AH is
  * decremented, and AF and CF are set; else AF and CF are cleared. In both cases the high nibble of AL is cleared. The
  * incoming CF is not an input to AAS.
  *
  * Returns `(al, ah, af, cf)`, in that order.
  */
def aas(al: Bus, ah: Bus, af: Port): Spec[(Bus, Bus, Port, Port)] = newSpec {
  assert(al.length == 8 && ah.length == 8, "AAS operates on 8-bit AL and AH")
  val adj = or(nibbleGt9(al.take(4)), af)
  val (alMinus6, _, _) = subtractor(al, const(8, 6))
  val alAdj = mux2(al, alMinus6, adj)
  val (ahMinus1, _) = decrementer(ah, adj)
  (alAdj.take(4) ++ Vector.fill(4)(Low), ahMinus1, adj, adj)
}
