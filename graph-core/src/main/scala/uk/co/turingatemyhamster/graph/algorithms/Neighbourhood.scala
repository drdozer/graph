package uk.co.turingatemyhamster.graph.algorithms

import uk.co.turingatemyhamster.graph.Graph
import uk.co.turingatemyhamster.collection.PriorityQueue
import scalaz._
import Scalaz._


/**
 * Created by IntelliJ IDEA.
 * User: nmrp3
 * Date: 27/01/12
 * Time: 00:11
 * To change this template use File | Settings | File Templates.
 */

trait Neighbourhood {

  def visit[V, E, P, W](start: V, fromVertex: V => Traversable[E], toVertex: E => V)
                       (implicit path: PathSpace[V, E, P, W]): Stream[P] = {
    
    def step(pq: PriorityQueue[W, P], seen: Set[V]): Stream[P] = {
      if(pq.isEmpty) Stream.Empty
      else {
        val ((_, best), rest) = pq.dequeue
        val last = path.last(best)
        val evs = fromVertex(last) map (e => e -> toVertex(e)) filter (ev => !seen(ev._2))
        val (nextQ, nextSeen) = evs.foldLeft((rest, seen)) { case ((q, s), (e, v)) =>
          val p = path.extend(best, e, v)
          (q.enqueue(path.cost(p), p), s + v)
        }
        Stream.cons(best, step(nextQ, nextSeen))
      }
    }

    val startP = path.startAt(start)
    step(PriorityQueue[W, P](path.cost(startP) -> startP)(Orderoid.reverse(path.costOrderoid), path.pathOrder), Set(start))

  }

}

class SG[F](f: (F, F) => F) extends Semigroup[F] {
  def append(f1: F, f2: => F) = f(f1, f2)
}

class Mon[F](sg: Semigroup[F], val zero: F) extends Semigroup[F] with Monoid[F] {
  def append(f1: F, f2: => F) = sg.append(f1, f2)
}

/**
 * An orderoid is an ordering and monoid over the same type, where the append operation is monotonic with respect to
 * the ordering.
 *
 * @tparam OM   the type this Orderoid is over
 * @author Matthew Pocock
 */
trait Orderoid[OM] extends Order[OM] with Monoid[OM]

object Orderoid {
  
  class OrderoidFromOrderAndMonoid[OM](ord: Order[OM], mon: Monoid[OM]) extends Orderoid[OM] {
    def append(s1: OM, s2: => OM) = mon.append(s1, s2)

    val zero = mon.zero

    def order(a1: OM, a2: OM) = ord.order(a1, a2)
  }

  private def doubleMult(a: Double, b: Double): Double = a * b
  private val doubleMultSemiG = new SG(doubleMult)

  /**
   * Orderoid for doubles x >= 1.
   */
  object LargeProductOrderoid extends OrderoidFromOrderAndMonoid[Double](
    implicitly[Order[Double]], new Mon(doubleMultSemiG, 1.0))

  @deprecated("Use Order.reverse once it lands in scalaz-seven snapshot")
  def reverse[T](o: Order[T]): Order[T] = new Order[T] {
    def order(a1: T, a2: T) = o.order(a2, a1)
  }
  
  /**
   * Orderoid for doubles 0 <= x <= 1
   */
  object FractionProductOrderoid extends OrderoidFromOrderAndMonoid[Double](
    reverse(implicitly[Order[Double]]), new Mon(doubleMultSemiG, 1.0))
}

/**
 * PathSpace typeclass. This encapsulates starting a path from a vertex, extending it by traversing an edge to another
 * vertex, and calculating the cumulative cost of the path. It also captures the monotonically increasing function
 * for combining costs, and an order for checking if two paths are equivalent.
 *
 * @tparam V  vertext type
 * @tparam E  edge type
 * @tparam P  path type
 * @tparam C  the cost type
 *
 * @author Matthew Pocock
 */
trait PathSpace[V, E, P, C] {
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
   * The cumulative cost of a path.
   *
   * @param p   the path
   * @return    the cumulative cost of the path `p`
   */
  def cost(p: P): C

  /**
   * Append costs in a monotonically increasing fashion.
   *
   * @return
   */
  def costOrderoid: Orderoid[C]

  /**
   * Ordering for checking if two paths are equivalent. If paths have the same cost, this will tie-break what order they
   * are searched in, from least to most.
   *
   * @return  an `Order` over paths of type `P`
   */
  def pathOrder: Order[P]
}