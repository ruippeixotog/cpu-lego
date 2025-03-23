package component

import scala.annotation.StaticAnnotation

import core.*

object BuilderAPI {

  trait BuilderEnv {
    def componentName: Option[String]

    def add(name: String, comp: Component): Unit
    def register(name: String, port: Port | Bus): Unit
    def wire(port1: Port, port2: Port): Unit
  }

  type Spec[A] = BuilderEnv ?=> A

  // port operations

  inline def newPort(): Port = ${ BuilderAPIMacros.newPort() }
  inline def newBus(inline n: Int): Bus = ${ BuilderAPIMacros.newBus('n) }

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
    var components = Map.empty[String, Component]
    var wires = List.empty[(Port, Port)]
    var namedPorts = Map.empty[String, Port | Bus]

    def candidateNames(name: String): Iterator[String] =
      Iterator(name) ++ Iterator.from(1).map(name + "$" + _)

    val env = new BuilderEnv {
      def componentName = name

      def add(name: String, comp: Component) = {
        candidateNames(name).find(!components.contains(_)).foreach { components += (_, comp) }
      }

      def register(name: String, port: Port | Bus) = namedPorts += (name, port)

      def wire(port1: Port, port2: Port) = wires = (port1, port2) :: wires
    }

    return (
      spec(using env),
      CompositeComponent(name.getOrElse(super.toString), components, wires, namedPorts)
    )
  }

  // annotations

  final class hidden extends StaticAnnotation
}
