package component

import core._

object BuilderAPI {

  trait BuilderEnv {
    def componentName: Option[String]

    def add(c: Component): Unit
    def wire(port1: Port, port2: Port): Unit
  }

  inline def newPort(): Port = ${ BuilderAPIMacros.newPortImpl() }

  inline def newComponent[A](inline build: BuilderEnv ?=> A)(using BuilderEnv): A =
    ${ BuilderAPIMacros.newComponentImpl('build) }

  def buildComponent[A](build: BuilderEnv ?=> A): (A, Component) =
    buildComponent(None, build)

  def buildComponent[A](name: Option[String], build: BuilderEnv ?=> A): (A, Component) = {
    var components0 = List.empty[Component]
    var wires0 = List.empty[(Port, Port)]

    given BuilderEnv = new BuilderEnv {
      def componentName = name

      def add(c: Component) = components0 = c :: components0
      def wire(port1: Port, port2: Port) = wires0 = (port1, port2) :: wires0
    }

    return (
      build,
      new CompositeComponent {
        val components = components0
        val wires = wires0

        override def toString = name.getOrElse(super.toString)
      }
    )
  }
}
