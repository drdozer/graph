package uk.co.turingatemyhamster.graph

class FiniteBinaryGraph[V, E] private (protected val verts: Set[V],
                                       protected val incd: Map[E, (V, V)],
                                       protected val vi: Map[V, Set[E]],
                                       protected val vo: Map[V, Set[E]])
  extends FiniteGraph[V, E] with BinaryGraph[V, E] {
  def edgesForVertex(v: V, i: Int) = i match {
    case 0 => edgesWhereVertexIsIncoming(v)
    case 1 => edgesWhereVertexIsOutgoing(v)
    case _ => throw new IndexOutOfBoundsException("Can't look up index %d in a pair".format(i))
  }

  def vertices: Col[V] = verts

  def edges: Col[E] = incd.keySet

  def incidence(e: E) = incd(e)

  def edgesWhereVertexIsIncoming(v: V): Col[E] = vi(v)

  def edgesWhereVertexIsOutgoing(v: V): Col[E] = vo(v)
}

object FiniteBinaryGraph {

  implicit def emptyGraph[V, E]: EmptyGraph[FiniteBinaryGraph[V, E]] = new EmptyGraph[FiniteBinaryGraph[V, E]] {
    def empty = new FiniteBinaryGraph[V, E](Set.empty, Map.empty, Map.empty, Map.empty)
  }

  implicit def appendVertices[V, E]
  : AppendVerticesToGraph[FiniteBinaryGraph[V, E], FiniteBinaryGraph[V, E], V] = new AppendVerticesToGraph[FiniteBinaryGraph[V, E], FiniteBinaryGraph[V, E], V] {
    def append(g: FiniteBinaryGraph[V, E], vs: Traversable[V]) = new FiniteBinaryGraph(g.verts ++ vs, g.incd, g.vi, g.vo)
  }
  
  implicit def appendEdges[V, E]
  : AppendEdgesToGraph[FiniteBinaryGraph[V, E], FiniteBinaryGraph[V, E], E, FiniteBinaryGraph#Incidence[V]] = new AppendEdgesToGraph[FiniteBinaryGraph[V, E], FiniteBinaryGraph[V, E], E, FiniteBinaryGraph#Incidence[V]] {
    def append(g: FiniteBinaryGraph[V, E], edgs: Traversable[(E, FiniteBinaryGraph#Incidence[V])]) = {

      var verts = g.verts
      var incd = g.incd
      var vi = g.vi
      var vo = g.vo

      for(p@(e, i) <- edgs) {
        val src = i._1
        val dst = i._2
        verts = verts + src + dst
        incd = incd + p
        vi = {
          vi + (src -> (vi get src match {
            case None => Set(e)
            case Some(es) => es + e
          }))
        }
        vo = {
          vo + (dst -> (vo get dst match {
            case None => Set(e)
            case Some(es) => es + e
          }))
        }
      }

      new FiniteBinaryGraph(verts, incd, vi, vo)

    }
  }
  
  implicit def removeTriples[V, E, S, L, D]
  (implicit triples: PathExpr[S, L, D] => Traversable[Triple[V, E]]): RemoveTriplesFromGraph[FiniteBinaryGraph[V, E],FiniteBinaryGraph[V, E], S,  L, D] = new RemoveTriplesFromGraph[FiniteBinaryGraph[V, E], FiniteBinaryGraph[V, E], S, L, D] {
    def remove(g: FiniteBinaryGraph[V, E], pe: PathExpr[S, L, D]) = {

      var incd = g.incd
      var vi = g.vi
      var vo = g.vo

      for(t <- pe) {
        incd = incd - t.label
        vi = {
          vi get t.source match {
            case None => vi
            case Some(es) =>
              es - t.label match {
                case ess if ess.isEmpty => vi - t.source
                case ess => vi + (t.source -> ess)
              }
          }
        }
        vo = {
          vo get t.destination match {
            case None => vo
            case Some(es) =>
              es - t.label match {
                case ess if ess.isEmpty => vo - t.destination
                case ess => vo + (t.destination -> ess)
              }
          }
        }
      }

      new FiniteBinaryGraph(g.verts, incd, vi, vo)
    }
  }
  
  implicit def removeVertices[V, E]: RemoveVerticesFromGraph[FiniteBinaryGraph[V, E], FiniteBinaryGraph[V, E], V] = new RemoveVerticesFromGraph[FiniteBinaryGraph[V, E], FiniteBinaryGraph[V, E], V] {
    def remove(g: FiniteBinaryGraph[V, E], vs: Traversable[V]) = new FiniteBinaryGraph[V, E](
      g.verts -- vs,
      g.incd filter { case (e, vv) => vs exists (v => v == vv._1 || v == vv._2) },
      g.vi filter { case (v, es) => vs exists (_ == v) },
      g.vo filter { case (v, es) => vs exists (_ == v) }
    )
  }
}