package component

import core._

object BuilderAPI {

  trait BuilderEnv {
    def componentName: Option[String]

    def add(name: String, comp: Component): Unit
    def register(name: String, port: Port | Vector[Port]): Unit
    def wire(port1: Port, port2: Port): Unit
  }

  type Bus = Vector[Port]

  type Spec[A] = BuilderEnv ?=> A

  // port operations

  inline def newPort(): Port = ${ BuilderAPIMacros.newPort() }
  inline def newBus(inline n: Int): Vector[Port] = ${ BuilderAPIMacros.newBus('n) }

  extension (self: Port) {
    inline def ~>(port: Port)(using env: BuilderEnv) =
      env.wire(self, port)
  }

  extension (self: Bus) {
    inline def ~>(ports: Bus)(using env: BuilderEnv) = {
      assert(self.length == ports.length, "Connected port vectors are not the same size")
      self.zip(ports).foreach(env.wire)
    }
  }

  // component operations

  inline def newSpec[A](inline spec: Spec[A])(using BuilderEnv): A =
    ${ BuilderAPIMacros.newSpec('spec) }

  def buildComponent[A](spec: Spec[A]): (A, Component) =
    buildComponent(None, spec)

  def buildComponent[A](name: Option[String], spec: Spec[A]): (A, Component) = {
    var components0 = Map.empty[String, Component]
    var namedPorts0 = Map.empty[String, Port | Vector[Port]]
    var wires0 = List.empty[(Port, Port)]

    def candidateNames(name: String): Iterator[String] =
      Iterator(name) ++ Iterator.from(1).map(name + "$" + _)

    val env = new BuilderEnv {
      def componentName = name

      def add(name: String, comp: Component) = {
        candidateNames(name).find(!components0.contains(_)).foreach { components0 += (_, comp) }
      }

      def register(name: String, port: Port | Vector[Port]) = namedPorts0 += (name, port)

      def wire(port1: Port, port2: Port) = wires0 = (port1, port2) :: wires0
    }

    return (
      spec(using env),
      new CompositeComponent {
        val components = components0
        val namedPorts = namedPorts0
        val wires = wires0

        override def toString = name.getOrElse(super.toString)
      }
    )
  }
}
