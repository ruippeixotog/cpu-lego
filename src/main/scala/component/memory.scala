package component

import component.BuilderAPI._
import core._

// non-intrinsic version of `flipflop`
def nandLatch(set: Port, reset: Port): Spec[(Port, Port)] = newComponent {
  val aux1, aux2 = newPort()
  val q = nand(reset, aux1)
  val nq = nand(set, aux2)
  q ~> aux2
  nq ~> aux1
  (q, nq)
}

def latchClocked(set: Port, reset: Port, clk: Port, preset: Port = High, clear: Port = High): Spec[(Port, Port)] =
  newComponent {
    flipflop(
      and(clear, nand(reset, clk)),
      and(preset, nand(set, clk))
    )
  }

def dLatch(in: Port, clk: Port, preset: Port = High, clear: Port = High): Spec[(Port, Port)] =
  newComponent {
    latchClocked(in, not(in), posEdge(clk), preset, clear)
  }

def jkMasterSlave(j: Port, k: Port, clk: Port, clear: Port): Spec[(Port, Port)] = newComponent {
  val aux1, aux2 = newPort()
  val (q, nq) = latchClocked(and(aux1, j), and(aux2, k), posEdge(clk), clear = clear)
  nq ~> aux1
  q ~> aux2
  (q, nq)
}

def register(xs: Seq[Port], load: Port, clk: Port, clear: Port = High): Spec[Seq[Port]] =
  newComponent {
    val notLoad = not(load)
    xs.map { x =>
      val aux = newPort()
      val (q, _) = dLatch(or(and(aux, notLoad), and(x, load)), clk, clear = clear)
      q ~> aux
      q
    }
  }

def bufferRegister(xs: Seq[Port], load: Port, enable: Port, clk: Port, clear: Port = High): Spec[Seq[Port]] =
  newComponent {
    register(xs, load, clk, clear).map(switch(_, enable))
  }

def counter(n: Int, count: Port, clk: Port, clear: Port): Spec[Seq[Port]] = newComponent {
  (1 to n).scanLeft(clk) { case (prev, _) => jkMasterSlave(count, count, not(prev), clear)._1 }.tail
}

def ringCounter(n: Int, clk: Port, clear: Port): Spec[Seq[Port]] = newComponent {
  val aux1, aux2 = newPort()
  val first = jkMasterSlave(aux1, aux2, clk, clear).swap
  val outs = (2 to n).scanLeft(first) { case ((q, nq), _) => jkMasterSlave(q, nq, clk, clear) }
  outs.last._1 ~> aux2
  outs.last._2 ~> aux1
  outs.map(_._1)
}
