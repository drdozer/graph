package uk.co.turingatemyhamster

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
package object graph {

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
