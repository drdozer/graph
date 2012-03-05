package uk.co.turingatemyhamster.graph

import algorithms.{PathSpace, Orderoid}
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mutable.Specification

import scalaz._
import Scalaz._

/**
 * Specification for the neighbourhood algorithms.
 *
 * @author Matthew Pocock
 */

@RunWith(classOf[JUnitRunner])
class NeighbourhoodSpec extends Specification {

  title("Neighbourhood algorithms")

  "the visit algorithm" should {

    "find the right path" in {

      val g = Graph.empty[FiniteBinaryGraph[Int, String]] appendVertices
        Seq(1,2,3,4,5) appendEdges
        Seq("a" -> (1 -> 2),
            "b" -> (1 -> 3),
            "c" -> (2 -> 3),
            "d" -> (3 -> 4),
            "e" -> (3 -> 5),
            "f" -> (4 -> 2))

      implicit def listOrder[A](implicit oa: Order[A]): Order[List[A]] = new Order[List[A]] {
        def order(x: List[A], y: List[A]) = (x, y) match {
          case (Nil, Nil) => Ordering.EQ
          case (as, Nil) => Ordering.GT
          case (Nil, bs) => Ordering.LT
          case (a::as, b::bs) => oa.order(a, b) match {
            case Ordering.EQ => order(as, bs)
            case o => o
          }
        }
      }

      implicit val listPath: PathSpace[Int, String, List[Int], Int] = new PathSpace[Int, String, List[Int], Int] {
        def startAt(start: Int) = start :: Nil

        def cost(p: List[Int]) = p.length

        def last(p: List[Int]) = p.head

        def extend(p: List[Int], edge: String, destination: Int) = destination :: p

        val costOrderoid = new Orderoid.OrderoidFromOrderAndMonoid[Int](
          implicitly[Order[Int]],
          implicitly[Monoid[Int]])

        val pathOrdering = Orderoid.reverse(implicitly[Order[List[Int]]])
      }

      val path = visit[FiniteBinaryGraph[Int, String], Int, String, List[Int], Int](
        g, 1,
        _.edgesWhereVertexIsIncoming(_),
        _.incidence(_).asInstanceOf[Tuple2[Int, Int]]._2)

      println(path.mkString(" ; "))

      path must_== Stream(1::Nil, 2::1::Nil, 3::1::Nil, 4::3::1::Nil, 5::3::1::Nil)
    }
  }

}
