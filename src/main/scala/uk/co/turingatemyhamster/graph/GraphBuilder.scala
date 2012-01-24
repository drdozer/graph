package uk.co.turingatemyhamster.graph

import annotation.implicitNotFound

/**
 * Empty graph.
 *
 * This graph contains no nodes or edges.
 *
 * To be compliant, `G <: Graph[V, E]` for some vertex and edge types.
 *
 * @author Matthew Pocock
 */
@implicitNotFound(msg = "Can not make an empty graph of type ${G}")
trait EmptyGraph[G] {
  def empty: G
}

/**
 * Builder for graphs from path expressions.
 *
 * The resulting graph contains exactly the triples described by the path expression.
 *
 * To be compliant, `G <: Graph[V, E]` for some vertex and edge types.
 *
 * @author Matthew Pocock
 */
@implicitNotFound(msg = "Can not build graph ${G} from ${S} <-< ${L} >-> ${D}")
trait GraphBuilder[G, S, L, D] {
  def buildGraph(pe: PathExpr[S, L, D]): G
}

object GraphBuilder {

  /** Implementation of graph builder that makes an empty gaph and then appends to it.*/
  implicit def usingZeroAppend[G, S, L, D]
  (implicit e: EmptyGraph[G],
    a: AppendTriplesToGraph[G, G, S, L, D]): GraphBuilder[G, S, L, D] = new GraphBuilder[G, S, L, D] {
    def buildGraph(pe: PathExpr[S, L, D]) = a.append(e.empty, pe)
  }

}

/**
 * Append vertices to a graph.
 *
 * The resulting graph contains the same triples as the original graph. It contains all vertices in the original graph
 * and those added.
 *
 * To be compliant, `G1 <: Graph[V, E]` and `G2 <: Graph[V, E]` for some edge types.
 *
 * @author Matthew Pocock
 */
trait AppendVerticesToGraph[G1, G2, V] {
  def append(g: G1, vs: Traversable[V]): G2
}

/**
 * Append edges to a graph.
 * 
 * The resulting graph contains the original vertices and edges, and in addition the added edges and any vertices
 * that they join.
 *
 * To be compliant, `G1 <: Graph[V, E]` and `G2 <: Graph[V, E]` for some edge types. Additionally,
 * `G1.Incidence <: G2.Incidence`.
 */
trait AppendEdgesToGraph[G1, G2, E, I] {
  def append(g: G1, es: Traversable[(E, I)]): G2
}

/**
 * Append triples to a graphs.
 *
 * The resulting graph contains exactly the triples in the original graph and also all of those in the path expression.
 * It contains all vertices in both the original graph and those mentioned in the added triples.
 *
 * To be compliant, `G1 <: Graph[V, E]` and `G2 <: Graph[V, E]` for some vertex and edge types.
 *
 * @author Matthew Pocock
 */
trait AppendTriplesToGraph[G1, G2, S, L, D] {
  def append(g: G1, pe: PathExpr[S, L, D]): G2
}

object AppendTriplesToGraph {

  /**
   * Implementation of appending triples where there is something that can make triples from the expression and a
   * way to add edges.
   */
  implicit def appendTriples[G1, G2, V, E, S, L, D, I]
  (implicit g1VE : G1 <:< Graph[V, E], g2VE : G2 <:< Graph[V, E],
   triples: PathExpr[S, L, D] => Traversable[Triple[V, E]],
   pairAsI: (V, V) => I,
   appEdges: AppendEdgesToGraph[G1, G2, E, I]): AppendTriplesToGraph[G1, G2, S, L, D] = new AppendTriplesToGraph[G1, G2, S, L, D] {
    def append(g: G1, pe: PathExpr[S, L, D]) = appEdges.append(g, pe map (t => (t.label, pairAsI(t.source, t.destination))))
  }

}

/**
 * Remove vertices from a graph.
 *
 * The resulting graph contains exactly the triples in the original graph that do not involve the removed vertices,
 * and contains all vertices in the original graph except for those removed.
 *
 * To be compliant, `G1 <: Graph[V, E]` and `G2 <: Graph[V, E]` for some edge types.
 *
 * @author Matthew Pocock
 */
trait RemoveVerticesFromGraph[G1, G2, V] {
  def remove(g: G1, vs: Traversable[V]): G2
}

/**
 * Remove edges from graph.
 *
 * The resulting graph contains exactly the edges in the original graph that are not also in the edge collection.
 * It contains all vertices in the original graph.
 *
 * To be compliant, `G1 <: Graph[V, E]` and `G2 <: Graph[V, E]` for some vertex type.
 *
 * @author Matthew Pocock
 */
trait RemoveEdgesFromGraph[G1, G2, E] {
  def remove(g: G1, es: Traversable[E]): G2
}

/**
 * Remove triples from a graph.
 *
 * The resulting graph contains exactly the triples in the original graph that are not also in the path expression.
 * It contains all vertices in the original graph.
 *
 * To be compliant, `G1 <: Graph[V, E]` and `G2 <: Graph[V, E]` for some vertex and edge types.
 *
 * @author Matthew Pocock
 */
trait RemoveTriplesFromGraph[G1, G2, S, L, D] {
  def remove(g: G1, pe: PathExpr[S, L, D]): G2
}

//object RemoveTriplesFromGraph {
//
//}