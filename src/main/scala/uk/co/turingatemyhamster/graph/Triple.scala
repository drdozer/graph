package uk.co.turingatemyhamster.graph

import annotation.implicitNotFound

/**
 * A triple is a labelled, directed edge linking a source and destination vertex.
 *
 * @author Matthew Pocock
 */
trait Triple[V, E] {
  def source: V
  def label: E
  def destination: V
}
