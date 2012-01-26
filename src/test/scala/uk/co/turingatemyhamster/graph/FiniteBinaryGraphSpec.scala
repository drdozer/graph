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
    
  }


}