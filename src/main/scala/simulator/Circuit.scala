package simulator

import core._
import util.UnionFind

case class PortGroup(root: Port)

case class Circuit(components: List[BaseComponent], wires: List[(Port, Port)]) {
  private lazy val uf = wires.foldLeft(UnionFind[Port]())(_.merge.tupled(_))

  lazy val groupOf: Port => PortGroup =
    uf.parents.keySet.map { p => (p, PortGroup(uf.root(p))) }.toMap.withDefault(PortGroup.apply)

  lazy val portsOf: PortGroup => Set[Port] =
    (uf.parents.keySet ++ uf.parents.values).groupBy(groupOf).withDefault { pg => Set(pg.root) }
}

object Circuit {
  def apply(root: Component, extraWires: List[(Port, Port)] = Nil): Circuit = {
    root match {
      case comp: BaseComponent => Circuit(List(comp), extraWires)
      case comp: CompositeComponent =>
        comp.components.values.map(apply(_)).fold(Circuit(Nil, extraWires ++ comp.wires)) { (c1, c2) =>
          Circuit(c1.components ++ c2.components, c1.wires ++ c2.wires)
        }
    }
  }
}
