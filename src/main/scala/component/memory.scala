package component

import component.BuilderDSL._
import core._

// non-intrinsic version of `flipflop`
def nandLatch(set: Port, reset: Port)(using env: BuilderEnv): (Port, Port) = {
  val aux1, aux2 = newPort()
  val q = nand(reset, aux1)
  val nq = nand(set, aux2)
  env.wire(q, aux2)
  env.wire(nq, aux1)
  (q, nq)
}

def latchClocked(set: Port, reset: Port, clk: Port, preset: Port = High, clear: Port = High)(using
    BuilderEnv
): (Port, Port) = {
  flipflop(
    and(clear, nand(reset, clk)),
    and(preset, nand(set, clk))
  )
}

def dLatch(in: Port, clk: Port, preset: Port = High, clear: Port = High)(using BuilderEnv): (Port, Port) = {
  latchClocked(in, not(in), posEdge(clk), preset, clear)
}

def jkMasterSlave(j: Port, k: Port, clk: Port, clear: Port)(using env: BuilderEnv): (Port, Port) = {
  val aux1, aux2 = newPort()
  val (q, nq) = latchClocked(and(aux1, j), and(aux2, k), posEdge(clk), clear = clear)
  env.wire(nq, aux1)
  env.wire(q, aux2)
  (q, nq)
}

def register(xs: Seq[Port], load: Port, clk: Port, clear: Port = High)(using env: BuilderEnv): Seq[Port] = {
  val notLoad = not(load)
  xs.map { x =>
    val aux = newPort()
    val (q, _) = dLatch(or(and(aux, notLoad), and(x, load)), clk, clear = clear)
    env.wire(q, aux)
    q
  }
}

def counter(n: Int, count: Port, clk: Port, clear: Port)(using BuilderEnv): Seq[Port] =
  (1 to n).scanLeft(clk) { case (prev, _) => jkMasterSlave(count, count, not(prev), clear)._1 }.tail
