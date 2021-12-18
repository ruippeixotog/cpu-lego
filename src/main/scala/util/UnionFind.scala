package util

case class UnionFind[A](parents: Map[A, A] = Map[A, A]()) {

  def root(elem: A): A = parents.get(elem) match {
    case None | Some(`elem`) => elem
    case Some(parent) => root(parent)
  }

  def merge(elem1: A, elem2: A): UnionFind[A] = UnionFind(
    parents + (root(elem1) -> root(elem2))
  )
}
