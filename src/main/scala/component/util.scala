package component

import core._
import component.BuilderDSL._

def multi(f: (Port, Port) => Port)(ports: Port*): Port =
  ports.reduce(f)
