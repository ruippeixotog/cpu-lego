package component

import component.BuilderDSL._
import core._

def norLatch(set: Port, reset: Port)(using env: BuilderEnv): (Port, Port) = {
  val aux1, aux2 = new Port
  val q = nor(reset, aux1)
  val nq = nor(set, aux2)
  env.wire(q, aux2)
  env.wire(nq, aux1)
  (q, nq)
}
