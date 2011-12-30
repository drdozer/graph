package uk.co.turingatemyhamster.graph

class FiniteBinaryGraph[V, E](verts: Set[V], incd: Map[E, (V, V)]) extends FiniteGraph[V, E] with BinaryGraph[V, E] {
  def edgesForVertex(v: V, i: Int) = i match {
    case 0 => edgesWhereVertexIsIncoming(v)
    case 1 => edgesWhereVertexIsOutgoing(v)
    case _ => throw new IndexOutOfBoundsException("Can't look up index %d in a pair".format(i))
  }

  def vertices: Col[V] = verts

  def edges: Col[E] = incd.keySet

  def incidence(e: E) = incd(e)

  def edgesWhereVertexIsIncoming(v: V): Col[E] = incd.filter(_._2._1 == v).map(_._1).toSet

  def edgesWhereVertexIsOutgoing(v: V): Col[E] = incd.filter(_._2._2 == v).map(_._1).toSet
}