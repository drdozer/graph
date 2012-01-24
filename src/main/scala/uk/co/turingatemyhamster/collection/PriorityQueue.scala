package uk.co.turingatemyhamster.collection

/**
 * A priority queue, that maintains a number of unique items in priority order and supports efficient removal of the
 * highest priority item and of enqueueing items.
 */

trait PriorityQueue[A, P] {
  
  def dequeue: ((A, P), PriorityQueue[A, P])

  def enqueue(ap: (A, P)): PriorityQueue[A, P]

  def enqueue(a: A, p: P): PriorityQueue[A, P] = enqueue(a -> p)

  def peek: (A, P) = dequeue._1

  def size: Int
  
  def isEmpty: Boolean
  
}