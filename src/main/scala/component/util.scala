package component

import component.BuilderAPI._
import core._

def multi(f: (Port, Port) => Port)(ports: Port*): Port =
  ports.reduce(f)

def buffered(xs: Seq[Port], enable: Port): Spec[Seq[Port]] =
  xs.map(switch(_, enable))
