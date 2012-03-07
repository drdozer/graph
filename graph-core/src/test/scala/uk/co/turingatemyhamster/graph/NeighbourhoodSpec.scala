package uk.co.turingatemyhamster.graph

import algorithms.{PathSpace, Orderoid}
import Orderoid.listOrder
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

    "find the right path for simple path counting" in {

      val g = Graph.empty[FiniteBinaryGraph[Int, String]] appendVertices
        Seq(1,2,3,4,5) appendEdges
        Seq("a" -> (1 -> 2),
            "b" -> (1 -> 3),
            "c" -> (2 -> 3),
            "d" -> (3 -> 4),
            "e" -> (3 -> 5),
            "f" -> (4 -> 2))

      implicit val listPath: PathSpace[Int, String, List[Int], Int] = new PathSpace[Int, String, List[Int], Int] {
        def startAt(start: Int) = start :: Nil

        def cost(p: List[Int]) = p.length

        def last(p: List[Int]) = p.head

        def extend(p: List[Int], edge: String, destination: Int) = destination :: p

        val costOrderoid = new Orderoid.OrderoidFromOrderAndMonoid[Int](
          implicitly[Order[Int]],
          implicitly[Monoid[Int]])

        val pathOrder = Orderoid.reverse(implicitly[Order[List[Int]]])
      }

      val path = visit(
        1,
        g.edgesWhereVertexIsIncoming(_: Int),
        g.incidence(_: String).asInstanceOf[Tuple2[Int, Int]]._2)

      println(path.mkString(" ; "))

      path must_== Stream(1::Nil, 2::1::Nil, 3::1::Nil, 4::3::1::Nil, 5::3::1::Nil)
    }

    case class WeightedEdge(id: Int, weight: Double)
    case class WeightedPath(path: List[Char], weight: Double)
    
    implicit val weOrd = Order.orderBy((_: WeightedEdge).id)
    
    implicit val listPath: PathSpace[Char, WeightedEdge, WeightedPath, Double] = new PathSpace[Char, WeightedEdge, WeightedPath, Double] {
      def startAt(start: Char) = WeightedPath(start :: Nil, costOrderoid.zero)
      
      def cost(p: WeightedPath) = p.weight
      
      def last(p: WeightedPath) = p.path.head
      
      def extend(p: WeightedPath, edge: WeightedEdge, destination: Char) = WeightedPath(destination :: p.path, costOrderoid.append(p.weight, edge.weight))
      
      def costOrderoid = Orderoid.FractionProductOrderoid
      
      def pathOrder = Order.orderBy((_:WeightedPath).path)(Orderoid.listOrder)
    }
    
    "find the right path for a weighted walk" in {

      val g = Graph.empty[FiniteBinaryGraph[Char, WeightedEdge]] appendVertices
        Seq('a', 'b', 'c', 'd') appendEdges
        Seq(WeightedEdge(1, 0.7) -> ('a' -> 'b'),
            WeightedEdge(2, 0.5) -> ('a' -> 'c'),
            WeightedEdge(3, 0.1) -> ('b' -> 'd'),
            WeightedEdge(4, 0.5) -> ('c' -> 'd'))
      
      val path = visit(
        'a',
        g.edgesWhereVertexIsIncoming(_: Char),
        g.incidence(_: WeightedEdge).asInstanceOf[Tuple2[Char, Char]]._2)
      
      println(path.mkString(" ; "))

      path must_== Stream(
        WeightedPath('a'::Nil, 1.0),
        WeightedPath('b'::'a'::Nil, 0.7),
        WeightedPath('c'::'a'::Nil, 0.5),
        WeightedPath('d'::'c'::'a'::Nil, 0.25))
    }
  }

}
