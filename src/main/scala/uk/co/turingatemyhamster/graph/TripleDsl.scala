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

/**
 * A path expression, which may represent many triples.
 *
 * @author Matthew Pocock
 */
case class PathExpr[+S, +L, +D](source: S, label: L, destination: D)

/**
 * A DSL for expressing path expressions and their conversion into equivalent triples.
 *
 * @author Matthew Pocock
 */
trait TripleDsl extends LowPriorityTripleDsl {

  implicit def manySources[S, L, D, V](implicit sv: S <:< Traversable[V]) : Sources[S, L, D, V] = new Sources[S, L, D, V] {
    def sources(pe: PathExpr[S, L, D]) = pe.source
  }

  implicit def tripleSources[S, L, D, V, E, PES, PEL, PED]
  (implicit st: S <:< PathExpr[PES, PEL, PED], sDests: Destinations[PES, PEL, PED, V]): Sources[S, L, D, V] = new Sources[S, L, D, V] {
    def sources(pe: PathExpr[S, L, D]) = sDests.destinations(pe.source)
  }

  implicit def manyLabels[S, L, D, E](implicit le: L <:< Traversable[E]): Labels[S, L, D, E] = new Labels[S, L, D, E] {
    def labels(pe: PathExpr[S, L, D]) = pe.label
  }

  implicit def manyDestinations[S, L, D, V](implicit dv: D <:< Traversable[V]): Destinations[S, L, D, V] = new Destinations[S, L, D, V] {
    def destinations(pe: PathExpr[S, L, D]) = pe.destination
  }

  implicit def tripleDeps[S, L, D, V, E, PES, PEL, PED]
  (implicit st: S <:< PathExpr[PES, PEL, PED], triples: S => Traversable[Triple[V, E]]): Deps[S, L, D, V, E] = new Deps[S, L, D, V, E] {
    def deps(pe: PathExpr[S, L, D]) = triples(pe.source)
  }

  implicit def toTriples[S, L, D, V, E]
  (pe: PathExpr[S, L, D])
  (implicit ss: Sources[S, L, D, V], ll: Labels[S, L, D, E], dd: Destinations[S, L, D, V],
   others: Deps[S, L, D, V, E]): Traversable[Triple[V, E]] =
    others.deps(pe) ++ (for(s <- ss.sources(pe); l <- ll.labels(pe); d <- dd.destinations(pe)) yield TripleImpl(s, l, d))

}

/**
 * The basic components of the triple DSL.
 *
 * This includes the implicit definitions that must be low-priority so as not to cause diverging implicit problems.
 *
 * @author Matthew Pocock
 */
trait LowPriorityTripleDsl {
  protected case class TripleImpl[V, E](source: V, label: E, destination: V) extends Triple[V, E]

  implicit def buildPathExpr[S](s: S): {def <-< [L] (l: L): { def >-> [D] (d: D): PathExpr[S, L, D]}} = new {
    def <-< [L] (l: L) = new {
      def >-> [D] (d: D) = new PathExpr(s, l, d)
    }
  }

  @implicitNotFound(msg="Could not extract ${V} sources from ${S} <-< ${L} >-> ${D}")
  trait Sources[S, L, D, V] {
    def sources(pe: PathExpr[S, L, D]): Traversable[V]
  }

  @implicitNotFound(msg="Could not extract ${E} labels from ${S} <-< ${L} >-> ${D}")
  trait Labels[S, L, D, E] {
    def labels(pe: PathExpr[S, L, D]): Traversable[E]
  }

  @implicitNotFound(msg="Could not extract ${V} destinations from ${S} <-< ${L} >-> ${D}")
  trait Destinations[S, L, D, V] {
    def destinations(pe: PathExpr[S, L, D]): Traversable[V]
  }

  @implicitNotFound(msg="Could not extract Triple[${V}, ${E}] from ${S} <-< ${L} >-> ${D}")
  trait Deps[S, L, D, V, E] {
    def deps(pe: PathExpr[S, L, D]): Traversable[Triple[V, E]]
  }

  implicit def oneSource[S, L, D]: Sources[S, L, D, S] = new Sources[S, L, D, S] {
    def sources(pe: PathExpr[S, L, D]) = pe.source :: Nil
  }

  implicit def oneLabel[S, L, D]: Labels[S, L, D, L] = new Labels[S, L, D, L] {
    def labels(pe: PathExpr[S, L, D]) = pe.label :: Nil
  }

  implicit def oneDestination[S, L, D]: Destinations[S, L, D, D] = new Destinations[S, L, D, D] {
    def destinations(pe: PathExpr[S, L, D]) = pe.destination :: Nil
  }

  implicit def noDeps[S, L, D, V, E]: Deps[S, L, D, V, E] = new Deps[S, L, D, V, E] {
    def deps(pe: PathExpr[S, L, D]) = Nil
  }

}