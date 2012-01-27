package uk.co.turingatemyhamster.graph.algorithms

import uk.co.turingatemyhamster.graph.Graph
import uk.co.turingatemyhamster.collection.PriorityQueue
import scalaz._
import Scalaz._
import scala.Ordering


/**
 * Created by IntelliJ IDEA.
 * User: nmrp3
 * Date: 27/01/12
 * Time: 00:11
 * To change this template use File | Settings | File Templates.
 */

class Neighbourhood {

  def visit[V, E, P, W](g: Graph[V, E], start: V, fromVertex: (Graph[V, E], V) => Traversable[E], toVertex: (Graph[V, E], E) => V)
                       (implicit roidP: Orderoid[W], pOrd: Ordering[P], path: Path[V, E, P, W]): Stream[P] = {
    
    def step(pq: PriorityQueue[W, P], seen: Set[V]): Stream[P] = {
      if(pq.isEmpty) Stream.Empty
      else {
        val ((_, best), rest) = pq.dequeue
        val last = path.last(best)
        val evs = fromVertex(g, last) map (e => e -> toVertex(g, e)) filter (ev => !seen(ev._2))
        val (nextQ, nextSeen) = evs.foldLeft((rest, seen)) { case ((q, s), (e, v)) =>
          val p = path.extend(best, e, v)
          (q.enqueue(path.weight(p), p), s + v)
        }
        Stream.cons(best, step(nextQ, nextSeen))
      }
    }

    val startP = path.startAt(start)
    step(PriorityQueue[W, P](path.weight(startP) -> startP), Set(start))

  }

}

/**
 * An orderoid is an ordering and monoid over the same type, where the append operation is monotonic with respect to
 * the ordering.
 *
 * @tparam OM   the type this Orderoid is over
 * @author Matthew Pocock
 */
trait Orderoid[OM] extends Ordering[OM] with Monoid[OM]

object Orderoid {
  
  class OrderoidFromOrderingAndMonoid[OM](ord: Ordering[OM], mon: Monoid[OM]) extends Orderoid[OM] {
    def append(s1: OM, s2: => OM) = mon.append(s1, s2)

    val zero = mon.zero

    def compare(x: OM, y: OM) = ord.compare(x, y)
  }

  private def doubleMult(a: Double, b: Double): Double = a * b
  private val doubleMultSemiG = semigroup(doubleMult)

  /**
   * Orderoid for doubles x >= 1.
   */
  object LargeProductOrderoid extends OrderoidFromOrderingAndMonoid[Double](
    implicitly[Ordering[Double]], Monoid.monoid(doubleMultSemiG, zero(1.0)))

  /**
   * Orderoid for doubles 0 <= x <= 1
   */
  object FractionProductOrderoid extends OrderoidFromOrderingAndMonoid[Double](
    implicitly[Ordering[Double]].reverse, Monoid.monoid(doubleMultSemiG, zero(1.0)))
}

/**
 * Path typeclass. This encapsulates starting a path from a vertex, extending it by traversing an edge to another
 * vertex, and calculating the cumulative weight of the path.
 *
 * @tparam V  vertext type
 * @tparam E  edge type
 * @tparam P  path type
 */
trait Path[V, E, P, W] {
  /**
   * Create a new path that includes just one vertex.
   *
   * @param start the starting vertex
   * @return      a path that walks only this one vertex
   */
  def startAt(start: V): P

  /**
   * Extend a path by traversing an edge to a vertex.
   *
   * @param p               the path to extend
   * @param edge            the edge traversed
   * @param destination     the vertex traversed to
   * @return                a path that is `p` extended by `edge` to `destination`
   */
  def extend(p: P, edge: E, destination: V): P

  /**
   * The last vertex in the path
   */
  def last(p: P): V

  /**
   * The cumulative weight of a path.
   *
   * @param p   the path
   * @return    the cumulative weight of the path `p`
   */
  def weight(p: P): W
}