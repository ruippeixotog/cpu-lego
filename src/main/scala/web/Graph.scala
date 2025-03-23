package web

case class Graph(
    nodes: List[(String, Node)],
    edges: List[Edge],
    initials: List[(InPortRef, Any)],
    subgraphs: List[(String, Graph)],
    externalIns: List[ExtInPortRef],
    externalOuts: List[ExtOutPortRef]
)

case class ExtInPortRef(
    portName: String,
    innerPort: InPortRef,
    `implicit`: Boolean
)

case class ExtOutPortRef(
    portName: String,
    innerPort: OutPortRef,
    `implicit`: Boolean
)

case class Edge(from: OutPortRef, to: InPortRef)

case class InPortRef(nodeId: String, portName: String)
case class OutPortRef(nodeId: String, portName: String)

sealed trait Node
case class ComponentNode(componentId: String) extends Node
case class SubgraphNode(subgraphId: String) extends Node
