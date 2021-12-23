package component

import core._

object BuilderDSL {

  trait BuilderEnv {
    def add(c: Component): Unit
    def wire(port1: Port, port2: Port): Unit
  }

  inline def newPort(): Port = ${ BuilderDSLMacros.newPortImpl() }

  def buildComponent[A](buildFunc: BuilderEnv => A): (A, Component) = {
    var components0 = List.empty[Component]
    var wires0 = List.empty[(Port, Port)]

    val res = buildFunc(
      new BuilderEnv {
        def add(c: Component) = components0 = c :: components0
        def wire(port1: Port, port2: Port) = wires0 = (port1, port2) :: wires0
      }
    )
    return (
      res,
      new CompositeComponent {
        val components = components0
        val wires = wires0
      }
    )
  }
}
