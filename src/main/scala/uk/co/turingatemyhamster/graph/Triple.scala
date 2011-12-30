package uk.co.turingatemyhamster.graph

import annotation.implicitNotFound

/**
 * A triple is a labelled, directed edge linking a source and destination vertex.
 *
 * @tparam V  vertex type
 * @tparam E  edge type
 *
 * @see [[uk.co.turingatemyhamster.graph.Graph]]
 *
 * @author Matthew Pocock
 */
trait Triple[V, E] {
  /** Source of the triple. */
  def source: V
  /** Label of the triple. */
  def label: E
  /** Destination of the triple. */
  def destination: V
}
