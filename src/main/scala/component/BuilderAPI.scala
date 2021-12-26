package component

import core._

object BuilderAPI {

  trait BuilderEnv {
    def componentName: Option[String]

    def add(c: Component): Unit
    def wire(port1: Port, port2: Port): Unit
  }

  type Spec[A] = BuilderEnv ?=> A

  // port operations

  inline def newPort(): Port = ${ BuilderAPIMacros.newPortImpl() }
  inline def newPortVec(inline n: Int): Vector[Port] = ${ BuilderAPIMacros.newPortVecImpl('n) }

  extension (self: Port) {
    inline def ~>(port2: Port)(using env: BuilderEnv) =
      env.wire(self, port2)
  }

  // component operations

  inline def newComponent[A](inline spec: Spec[A])(using BuilderEnv): A =
    ${ BuilderAPIMacros.newComponentImpl('spec) }

  def buildComponent[A](spec: Spec[A]): (A, Component) =
    buildComponent(None, spec)

  def buildComponent[A](name: Option[String], spec: Spec[A]): (A, Component) = {
    var components0 = List.empty[Component]
    var wires0 = List.empty[(Port, Port)]

    val env = new BuilderEnv {
      def componentName = name

      def add(c: Component) = components0 = c :: components0
      def wire(port1: Port, port2: Port) = wires0 = (port1, port2) :: wires0
    }

    return (
      spec(using env),
      new CompositeComponent {
        val components = components0
        val wires = wires0

        override def toString = name.getOrElse(super.toString)
      }
    )
  }
}
