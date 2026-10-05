package simulator

import core.*

/** A group of ports joined by wires: every port on the same net, named by a canonical representative port. */
case class PortGroup(root: Port)

/** A flattened circuit: the primitive components in depth-first traversal order, the original wire pairs, and the flat
  * [[Netlist]] they were built from. Port groups and net membership derive from the netlist, so flattening is linear in
  * the size of the hierarchy instead of quadratic.
  */
final class Circuit private (
    val components: List[BaseComponent],
    val wires: List[(Port, Port)],
    private val netlist: Netlist
) {

  /** The flat netlist this circuit was built from. */
  def toNetlist: Netlist = netlist

  /** The net group `port` belongs to. Ports unknown to the circuit form singleton groups. */
  lazy val groupOf: Port => PortGroup =
    (port: Port) => PortGroup(netlist.representativeOf(port).getOrElse(port))

  /** Every port on the same net as `group.root`. */
  lazy val portsOf: PortGroup => Set[Port] =
    (group: PortGroup) => netlist.netPorts(group.root).getOrElse(Set(group.root))
}

object Circuit {

  /** Build from an already-flat component and wire list. */
  def apply(components: List[BaseComponent], wires: List[(Port, Port)]): Circuit = {
    val netlist = Netlist.fromFlat(components, wires)
    new Circuit(netlist.baseComponents, wires, netlist)
  }

  /** Flatten a component hierarchy. The traversal is iterative and linear; `components` come out in depth-first
    * traversal order and `wires` keep their original pairing (`extraWires` first).
    */
  def apply(root: Component, extraWires: List[(Port, Port)] = Nil): Circuit = {
    val netlist = Netlist(root, extraWires)
    val wires = {
      val b = List.newBuilder[(Port, Port)]
      var i = 0
      while (i < netlist.wireA.length) {
        b += ((netlist.ports(netlist.wireA(i)), netlist.ports(netlist.wireB(i))))
        i += 1
      }
      b.result()
    }
    new Circuit(netlist.baseComponents, wires, netlist)
  }
}
