package uk.co.turingatemyhamster.collection

import collection.immutable.SortedSet

/**
 * A priority queue, that maintains a number of unique items in priority order and supports efficient removal of the
 * highest priority item and of enqueueing items. In the event of a priority draw, the order in which items are dequeued
 * depends upon the implementation. Each enqueued item appears at most once in the queue.
 *
 * @author Matthew Pocock
 */

trait PriorityQueue[P, A] {
  
  def dequeue: ((P, A), PriorityQueue[P, A])

  def enqueue(ap: (P, A)): PriorityQueue[P, A]

  def enqueue(p: P, a: A): PriorityQueue[P, A] = enqueue(p -> a)

  def peek: (P, A) = dequeue._1

  def isEmpty: Boolean
  
}

object PriorityQueue {
  
  def apply[P, A](pas: (P, A)*)(implicit pOrd: Ordering[P], aOrd: Ordering[A]): PriorityQueue[P, A] =
    pas.foldLeft(new PQ[P, A](SortedSet()(new Ordering[(P, A)] {
      val paOrd = Ordering.Tuple2[P, A]
      def compare(x: (P, A), y: (P, A)) = if(aOrd.equiv(x._2, y._2)) 0 else paOrd.compare(x, y)
    })))(_ enqueue _)

  private case class PQ[P, A](q: SortedSet[(P, A)])
    extends PriorityQueue[P, A]
  {
    def dequeue = {
      val last = q.lastKey
      (last, copy(q = q - last))
    }

    def enqueue(pa: (P, A)) = {
      copy(q = q + pa)
    }

    def isEmpty = q.isEmpty
  }
}