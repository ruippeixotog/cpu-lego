package component

import component.BuilderAPI._
import core._

def multi(f: (Port, Port) => Port)(ports: Port*): Port =
  ports.reduce(f)

def buffered(enable: Port)(xs: Bus): Spec[Bus] =
  xs.map(switch(_, enable))
