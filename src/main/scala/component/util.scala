package component

import core._

def multi(f: (Port, Port) => Port)(ports: Port*): Port =
  ports.reduce(f)
