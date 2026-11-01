package component

import component.BuilderAPI.*
import core.*

def multi(f: (Port, Port) => Port)(ports: Port*): Port =
  ports.reduce(f)

def buffered[N <: Int](enable: Port)(xs: Bus[N]): Spec[Bus[N]] = newSpec {
  xs.map(switch(_, enable))
}
