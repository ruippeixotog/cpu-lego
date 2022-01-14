package simulator

import core._

case class Index(components: Map[String, Component], ports: Map[String, Port], buses: Map[String, Bus])

object Index {

  def apply(root: Component, path: String = ""): Index = {
    root match {
      case comp: BaseComponent =>
        Index(Map(path -> comp), Map(), Map())

      case comp: CompositeComponent =>
        val prefix = if (path.isEmpty) path else s"$path."

        val (ports, buses) = comp.namedPorts.toList.map {
          case (name, port: Port) => (Some(prefix + name -> port), None)
          case (name, bus: Bus) => (None, Some(prefix + name -> bus))
        }.unzip

        val idx = Index(Map(path -> comp), ports.flatten.toMap, buses.flatten.toMap)
        val children = comp.components.map { case (name, c) => apply(c, prefix + name) }

        children.fold(idx) { (idx1, idx2) =>
          Index(idx1.components ++ idx2.components, idx1.ports ++ idx2.ports, idx1.buses ++ idx2.buses)
        }
    }
  }
}
