package uk.co.turingatemyhamster.graph

/**
 * Graphs are central to many algorithms and to solving many real-world problems. This package provides an API for
 * working with various families of graph representations, while maximising the freedom of implementors to implement
 * graph storage and operations.
 *
 * <h2>Motivation</h2>
 *
 * In many applications of graphs, the vertices and edges represent interesting, application-domain values such as
 * places and roads, proteins and interactions, people and published papers. This API seeks to recognise this from
 * the outset, so that graphs are directly represented over their application-domain entities, without the need to
 * modify these or derive custom graph implementations.
 *
 * In typical cases where application-domain values are represented as graphs, there are many graphs for the same
 * values. For example, people could be arranged into a graph by marriage, family trees, friendship. Given a graph
 * of people who phone each other, there are many graphs to be had by filtering the phonecalls by time of day,
 * date range, call length. However, from the point of view of the application, it probably makes sense to use the same
 * vertex and edge application-domain instances in these distinct graphs.
 * 
 * To support this, the `Graph` API uses type-classes to expose the graph structure, distinct from the graph, vertex or
 * edge types themselves.
 *
 * <h2>Maths</h2>
 *
 * The most common mathematical definition of a graph is: `G = {V, E⊆(V⨯V)}`. However, there are many related structures
 * that are sometimes called graphs, or given more specific names.
 *
 * To account for this, the `Graph` type-class actually represents a slightly different structure:
 *
 *   `G = {V, E, incidence:E->(V⨯V) }`
 *
 * where `V` and `E` are the application-domain values and incidence is a function that maps from each `E` to the
 * associated vertices.
 * This, in effect, hides the underlying structure of the vertices and edges, letting us describe all common graph
 * operations purely in terms of the application-domain values. Implementors are free to represent the graph in terms
 * of a graph over some other types `V'` and `E'` and a bijective mapping `Iv:V->V'`, `Ie:E->E'`.
 *
 * The graph structure described so far is a binary, directed graph. The incidence range is an ordered pair.
 * If the graph is binary and undirected, then the incidence range is an unordered bag of cardinality 2.
 * If it is binary, undirected and without loops, then incidence range is sets of cardinality 2.
 * In a hypergraph, each edge can connect any number of vertices.
 * Here, the incidence type is a list of vertices (a bag, in the case of an un-directed graph).
 * Pseudographs require the incidence function to allow multiple edges to map to the same incident vertices.
 * True graphs require the incidence function to be bijective, so that it uniquely associates edges with their incident
 * vertices.
 *
 * So, a generalized graph is some vertices, edges, and an incidence function and type, together with some restrictions
 * on the incidence function.
 *
 * `G = {Inc, V, E, incidence: E->Inc[V]}`
 *
 * Lastly, some graphs are infinite in extent while others are finite. For the finite case, it makes sense for the
 * vertex and edge sets to be represented directly by `Set` datastructures. For infinite representations, there are
 * other alternatives.
 * Sometimes it will be a cheep operation to see if a value is in one of these sets. Here, representing the collection
 * by its indicator function is sufficient.
 * Other times, it may be possible to iterate over members in some order, given that the iteration may not terminate.
 * Here, an `Iterable` is appropriate.
 * It's worth noting in passing that `Set` is both `Iterable` and has an `apply()` method that is an indicator
 * function.
 *
 * @author Matthew Pocock
 */
package object Graph {

  /** A graph where nodes, edges and links between these can be checked with instances to see if they are in these
   * collections.
   */
  type QueryableMembershipGraph[G, V, E] = Graph[G, V, E] {

    type Col[A] <: A => Boolean

  }

  /**
   * A graph where nodes, edges and links between these can be iterated over.
   *
   * If a graph is finite and navigable, iteration will at some point terminate. If a graph is infinite and navigable,
   * then no individual iterator is guaranteed to terminate.
   *
   * @author Matthew Pocock
   */
  type NavigableGraph[G, V, E] = Graph[G, V, E] {

    type Col[A] <: Iterable[A]
  }
}

/**
 * A Graph type-class.
 * 
 * This supports graphs of both infinite and finite size.
 *
 * @tparam G  the underlying datatype for the graph
 * @tparam V  the vertex type
 * @tparam E  the edge type
 * 
 * @author Matthew Pocock
 */
trait Graph[G, V, E] {
  
  /** Type representing 'collections' of vertices or edges. */
  type Col[A]
  
  /** Type representing incident vertices for an edge. */
  type Incidence[A]
  
  def vertices(g: G): Col[V]
  def edges(g: G): Col[E]
  
  /** The incident vertices for an edge. This is only defined for edges that are in the graph. */
  def incidence(g: G, e: E): Incidence[V]
  
  /** The incident vertices for an edge. `None` for edges not in the graph. */
  def incidenceOption(g: G, e: E): Option[Incidence[V]]
}

/**
 * A graph over a finite set of nodes and a finite set of edges.
 * 
 * @author Matthew Pocock
 */
trait FiniteGraph[G, V, E] extends Graph[G, V, E] {

  /** In a finite graph, the vertices and edges associated with each other are sets. */
  type Col[A] = Set[A]
  
  def incidenceOption(g: G, e: E): Option[Incidence[V]] = if(edges(g)(e)) Some(incidence(g, e)) else None
}

/**
 * An undirected graph without loops.
 *
 * @author Matthew Pocock
 */
trait UndirectedGraph[G, V, E] extends Graph[G, V, E] {

  import collection.immutable.Set.Set2

  /** In an undirected graph, incidence is always a two-element set. */
  type Incidence[A] = Set2[A]

}

/**
 * A directed graph. The incidence type respects ordering.
 *
 * @author Matthew Pocock
 */
trait DirectedGraph[G, V, E] extends Graph[G, V, E] {
  /**
   * Access elements of incidence by index. Indices count from 0.
   *
   * @throws IndexOutOfBoundsException if the index does not fall witin the incidence structure
   */
  def at[A](ic: Incidence[A], i: Int): A

  /**
   * Edges associated with a vertex, given some index into the incidence structure.
   *
   * @param g graph
   * @param v vertex
   * @param i index into the incidence of v in g
   * @return  all edges that contain `v` at position `i` in their incidence
   */
  def edgesForVertex(g: G, v: V, i: Int): Col[E]
}

/**
 * A binary graph. This is the common form of a (multi-)graph, where edges are directed and link pairs of
 * vertices.
 *
 * @author Matthew Pocock
 */
trait BinaryGraph[G, V, E] extends DirectedGraph[G, V, E] {

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
   * This is equivalent to edgesForVertex(g, v, 0)
   */
  def edgesWhereVertexIsIncoming(g: G, v: V): Col[E]

  /**
   * Edges where the vertex is in the outgoing position of the incidence.
   *
   * This is equivalent to edgesForVertex(g, v, 1)
   */
  def edgesWhereVertexIsOutgoing(g: G, v: V): Col[E]
}

/**
 * A hypergraph.
 *
 * @author Matthew Pocock
 */
trait Hypergraph[G, V, E] extends DirectedGraph[G, V, E] {

  /** In a hypergraph, incidence is an indexed sequence. */
  type Incidence[A] = IndexedSeq[A]

  def at[A](ic: Incidence[A], i: Int) = ic(i)
}