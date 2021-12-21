package component

import component.BuilderDSL._
import core._

def nandLatch(set: Port, reset: Port)(using env: BuilderEnv): (Port, Port) = {
  val aux1, aux2 = new Port
  val q = nand(reset, aux1)
  val nq = nand(set, aux2)
  env.wire(q, aux2)
  env.wire(nq, aux1)
  (q, nq)
}

def latchClocked(set: Port, reset: Port, clk: Port, preset: Port = High, clear: Port = High)(using
    BuilderEnv
): (Port, Port) = {
  nandLatch(
    and(clear, nand(reset, clk)),
    and(preset, nand(set, clk))
  )
}

def dLatch(in: Port, clk: Port, preset: Port = High, clear: Port = High)(using BuilderEnv): (Port, Port) = {
  latchClocked(in, not(in), posEdge(clk), preset, clear)
}
