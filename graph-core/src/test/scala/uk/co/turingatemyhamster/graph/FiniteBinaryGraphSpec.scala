package uk.co.turingatemyhamster.graph

import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import org.specs2.mutable.Specification

@RunWith(classOf[JUnitRunner])
class FiniteBinaryGraphSpec extends Specification {

  title("A finite binary graph implementation")

  "an empty graph" should {
    
    val empty = Graph.empty[FiniteBinaryGraph[Int, String]]
    
    "contain no vertices" in {
      empty.vertices.isEmpty must_== true
    }
    
    "contain no edges" in {
      empty.edges.isEmpty must_== true
    }

    "contain one vertex when a vertex is added" in {
      val g = empty appendVertices Seq(1)
      g.vertices must contain(1)
    }

    "contain several vertices when they are added" in {
      val g = empty appendVertices Seq(1, 2, 3, 4)
      g.vertices must contain(1, 2, 3, 4)
    }
  }

  "a binary, directed graph" should {
    val g = Graph.empty[FiniteBinaryGraph[Int, String]] appendVertices
      Seq(1,2,3,4,5) appendEdges
      Seq("a" -> (1 -> 2),
          "b" -> (1 -> 3),
          "c" -> (2 -> 3),
          "d" -> (3 -> 4),
          "e" -> (3 -> 5),
          "f" -> (4 -> 2))

    "contain the right vertices" in {
      g.vertices must containAllOf(Seq(1,2,3,4,5))
    }

    "contain the right edges" in {
      g.edges must containAllOf(Seq("a", "b", "c", "d", "e"))
    }

    "edge a is incident with 1 and 2" in {
      g.incidence("a") must_== (1, 2)
    }

    "edge b is incident with 1 and 3" in {
      g.incidence("b") must_== (1,3)
    }

    "edge c is incident with 2 and 3" in {
      g.incidence("c") must_== (2,3)
    }

    "edge d is incident with 3 and 4" in {
      g.incidence("d") must_== (3,4)
    }

    "edge e is incident with 3 and 5" in {
      g.incidence("e") must_== (3,5)
    }

    "edge f is incident with 4 and 2" in {
      g.incidence("f") must_== (4,2)
    }

    "vertex 1 has edges where it is the incomming node for a, b" in {
      g.edgesWhereVertexIsIncoming(1) must_== Set("a", "b")
    }

    "vertex 2 has edges where it is the incomming node for c" in {
      g.edgesWhereVertexIsIncoming(2) must_== Set("c")
    }

    "vertex 3 has edges where it is the incomming node for d, e" in {
      g.edgesWhereVertexIsIncoming(3) must_== Set("d", "e")
    }

    "vertex 4 has edges where it is the incomming node for f" in {
      g.edgesWhereVertexIsIncoming(4) must_== Set("f")
    }

    "vertex 5 has edges where it is the incomming node for none" in {
      g.edgesWhereVertexIsIncoming(5) must_== Set()
    }

    "vertex 1 has edges where it is the outcoing node for none" in {
      g.edgesWhereVertexIsOutgoing(1) must_== Set()
    }

    "vertex 2 has edges where it is the outcoing node for a, f" in {
      g.edgesWhereVertexIsOutgoing(2) must_== Set("a", "f")
    }

    "vertex 3 has edges where it is the outcoing node for b, c" in {
      g.edgesWhereVertexIsOutgoing(3) must_== Set("b", "c")
    }

    "vertex 4 has edges where it is the outcoing node for d" in {
      g.edgesWhereVertexIsOutgoing(4) must_== Set("d")
    }

    "vertex 5 has edges where it is the outcoing node for e" in {
      g.edgesWhereVertexIsOutgoing(5) must_== Set("e")
    }
  }
}