package component

import component.BuilderAPI.*
import core.*

def multi(f: (Port, Port) => Port)(ports: Port*): Port =
  ports.reduce(f)

def buffered(enable: Port)(xs: Bus): Spec[Bus] = newSpec {
  xs.map(switch(_, enable))
}
