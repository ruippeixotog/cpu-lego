package component

import component.BuilderAPI._
import core._

def multi(f: (Port, Port) => Port)(ports: Port*): Port =
  ports.reduce(f)

def buffered(enable: Port)(xs: Seq[Port]): Spec[Seq[Port]] =
  xs.map(switch(_, enable))
