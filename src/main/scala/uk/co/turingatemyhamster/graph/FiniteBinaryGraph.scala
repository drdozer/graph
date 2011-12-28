package uk.co.turingatemyhamster.graph

class FiniteBinaryGraph[V, E](vertices: Set[V], incd: Map[E, (V, V)]) extends FiniteGraph[V, E] with BinaryGraph[V, E] {
  def edgesForVertex(v: V, i: Int) = i match {
    case 0 => edgesWhereVertexIsIncoming(v)
    case 1 => edgesWhereVertexIsOutgoing(v)
    case _ => throw new IndexOutOfBoundsException("Can't look up index %d in a pair".format(i))
  }

  def edges = incd.keySet

  def incidence(e: E) = incd(e)

  def edgesWhereVertexIsIncoming(v: V) = incd.iterator.filter(_._1._1 == v).map(_._1)

  def edgesWhereVertexIsOutgoing(v: V) = incd.iterator.filter(_._1._2 == v).map(_._1)
}