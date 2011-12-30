package uk.co.turingatemyhamster.graph

import annotation.implicitNotFound
import collection.immutable.LinearSeq

trait GraphOps {
  
}

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

object TripleDsl {
  private case class TripleImpl[V, E](source: V, label: E, destination: V) extends Triple[V, E]

  case class PathExpr[+S, +L, +D](source: S, label: L, destination: D)

  implicit def fromSrc[S](s: S) = new {
    def <-< [L] (l: L) = new {
      def >-> [D] (d: D) = new PathExpr(s, l, d)
    }
  }

  @implicitNotFound(msg="Could not extract ${V} sources from ${S} <-< ${L} >-> ${D}")
  trait Sources[S, L, D, V] extends Function1[PathExpr[S, L, D], Traversable[V]]

  @implicitNotFound(msg="Could not extract ${E} labels from ${S} <-< ${L} >-> ${D}")
  trait Labels[S, L, D, E] extends Function1[PathExpr[S, L, D], Traversable[E]]

  @implicitNotFound(msg="Could not extract ${V} destinations from ${S} <-< ${L} >-> ${D}")
  trait Destinations[S, L, D, V] extends Function1[PathExpr[S, L, D], Traversable[V]]

  @implicitNotFound(msg="Could not extract Triple[${V}, ${E}] from ${S} <-< ${L} >-> ${D}")
  trait Deps[S, L, D, V, E] extends Function1[PathExpr[S, L, D], Traversable[Triple[V, E]]]
  
  implicit def oneSource[S, L, D]: Sources[S, L, D, S] = new Sources[S, L, D, S] {
    def apply(pe: PathExpr[S, L, D]) = pe.source :: Nil
  }

  implicit def manySources[L, D, V]: Sources[Traversable[V], L, D, V] = new Sources[Traversable[V], L, D, V] {
    def apply(pe: PathExpr[Traversable[V], L, D]) = pe.source
  }

  implicit def oneLabel[S, L, D]: Labels[S, L, D, L] = new Labels[S, L, D, L] {
    def apply(pe: PathExpr[S, L, D]) = pe.label :: Nil
  }
  
  implicit def manyLabels[S, D, E]: Labels[S, Traversable[E], D, E] = new Labels[S, Traversable[E], D, E] {
    def apply(pe: PathExpr[S, Traversable[E], D]) = pe.label
  }

  implicit def oneDestination[S, L, D]: Destinations[S, L, D, D] = new Destinations[S, L, D, D] {
    def apply(pe: PathExpr[S, L, D]) = pe.destination :: Nil
  }

  implicit def manyDestinations[S, L, V]: Destinations[S, L, Traversable[V], V] = new Destinations[S, L, Traversable[V], V] {
    def apply(pe: PathExpr[S, L, Traversable[V]]) = pe.destination
  }

  implicit def noDeps[S, L, D, V, E]: Deps[S, L, D, V, E] = new Deps[S, L, D, V, E] {
    def apply(v1: PathExpr[S, L, D]) = Nil
  }


  implicit def toTriples[S, L, D, V, E]
  (pe: PathExpr[S, L, D])
  (implicit ss: Sources[S, L, D, V], ll: Labels[S, L, D, E], dd: Destinations[S, L, D, V],
   others: Deps[S, L, D, V, E]): Traversable[Triple[V, E]] =
    others(pe) ++ (for(s <- ss(pe); l <- ll(pe); d <- dd(pe)) yield TripleImpl(s, l, d))

  val pim = "pig" <-< "in" >-> "mud"
  val pigInMud: Traversable[Triple[String, String]] = pim

  val birdsNotFly: Traversable[Triple[String, String]] = List("penguine", "ostritch") <-< "does not" >-> "fly"
  val wifeIs: Traversable[Triple[String, String]] = "wife" <-< List("amazes", "astounds") >-> "me"
  val catfood: Traversable[Triple[String, String]] = "cat" <-< "eats" >-> List("mice", "birds")

  val rout: Traversable[Triple[String, String]] = "london" <-< "m11" >-> "cambridge" <-< "a1" >-> "nottingham"
  
}