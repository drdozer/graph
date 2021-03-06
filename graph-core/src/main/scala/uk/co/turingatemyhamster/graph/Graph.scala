package uk.co.turingatemyhamster.graph

/**
 * A Graph type-class.
 * 
 * This supports graphs of both infinite and finite size.
 *
 * @tparam V  the vertex type
 * @tparam E  the edge type
 * 
 * @author Matthew Pocock
 */
trait Graph[V, E] {
  
  /** Type representing 'collections' of vertices or edges. */
  type Col[A]
  
  /** Type representing incident vertices for an edge. */
  type Incidence[A]
  
  def vertices: Col[V]
  def edges: Col[E]
  
  /** The incident vertices for an edge. This is only defined for edges that are in the graph. */
  def incidence(e: E): Incidence[V]
  
  /** The incident vertices for an edge. `None` for edges not in the graph. */
  def incidenceOption(e: E): Option[Incidence[V]]
}

object Graph {
  def empty[G](implicit eg: EmptyGraph[G]): G = eg.empty
  implicit def asMutable[G](g: G): MutableGraph[G] = new MutableGraph(g)
}

/**
 * A graph over a finite set of nodes and a finite set of edges.
 * 
 * @author Matthew Pocock
 */
trait FiniteGraph[V, E] extends Graph[V, E] {

  /** In a finite graph, the vertices and edges associated with each other are sets. */
  type Col[A] = Set[A]
  
  def incidenceOption(e: E): Option[Incidence[V]] = if(edges(e)) Some(incidence(e)) else None
}

/**
 * An undirected graph without loops.
 *
 * @author Matthew Pocock
 */
trait UndirectedGraph[V, E] extends Graph[V, E] {

  import collection.immutable.Set.Set2

  /** In an undirected graph, incidence is always a two-element set. */
  type Incidence[A] = Set2[A]

}

/**
 * A directed graph. The incidence type respects ordering.
 *
 * @author Matthew Pocock
 */
trait DirectedGraph[V, E] extends Graph[V, E] {
  /**
   * Access elements of incidence by index. Indices count from 0.
   *
   * @throws IndexOutOfBoundsException if the index does not fall witin the incidence structure
   */
  def at[A](ic: Incidence[A], i: Int): A

  /**
   * Edges associated with a vertex, given some index into the incidence structure.
   *
   * @param v vertex
   * @param i index into the incidence of v in g
   * @return  all edges that contain `v` at position `i` in their incidence
   */
  def edgesForVertex(v: V, i: Int): Col[E]
}

/**
 * A binary graph. This is the common form of a (multi-)graph, where edges are directed and link pairs of
 * vertices.
 *
 * @author Matthew Pocock
 */
trait BinaryGraph[V, E] extends DirectedGraph[V, E] {

  type Incidence[A] = (A, A)

  /** In a binary directed graph, incidence is an ordered pair. */
  def at[A](ic: (A, A), i: Int) = i match {
    case 0 => ic._1
    case 1 => ic._2
    case _ => throw new IndexOutOfBoundsException("Can't look up index %d in a pair".format(i))
  }

  /**
   * Edges where the vertex is in the incomming position of the incidence.
   *
   * This is equivalent to edgesForVertex(v, 0)
   */
  def edgesWhereVertexIsIncoming(v: V): Col[E]

  /**
   * Edges where the vertex is in the outgoing position of the incidence.
   *
   * This is equivalent to edgesForVertex(v, 1)
   */
  def edgesWhereVertexIsOutgoing(v: V): Col[E]
}

/**
 * A hypergraph.
 *
 * @author Matthew Pocock
 */
trait Hypergraph[V, E] extends DirectedGraph[V, E] {

  /** In a hypergraph, incidence is an indexed sequence. */
  type Incidence[A] = IndexedSeq[A]

  def at[A](ic: Incidence[A], i: Int) = ic(i)
}