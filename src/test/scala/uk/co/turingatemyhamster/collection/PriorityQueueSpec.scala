package uk.co.turingatemyhamster.collection

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

/**
 * Specification for priority queues.
 *
 * @author Matthew Pocock
 */

@RunWith(classOf[JUnitRunner])
class PriorityQueueSpec extends Specification {

  title("A priority Queue")

  "an empty priority queue" should {

    "return true for isEmpty" in {
      PriorityQueue[Int, String]().isEmpty must_== true
    }

    "when an element is added" in {
      val pq = PriorityQueue[Int, String]() enqueue(1, "pig")

      "return false for isEmpty" in {
        pq.isEmpty must_== false
      }

      "deque to a pair consisting of" in {
        val (pa, q) = pq.dequeue

        "the enqueued item and priority" in {
          pa must_== (1, "pig")
        }

        "the empty queue" in {
          q.isEmpty must_== true
        }
      }
    }
  }

  "when the same item is enqueued twice under different priorities" should {
    val pq = PriorityQueue(1 -> "charlie", 2 -> "charlie")

    "dequeue to a pair consisting of" in {
      val (pa, q) = pq.dequeue

      "the item" in {
        pa._2 must_== "charlie"
      }

      "the highest priority" in {
        pa._1 must_== 2
      }

      "the empty queue" in {
        q must_== PriorityQueue[Int, String]()
      }
    }
  }

  "queues containing multiple items at the same priority" should {
    var pq = PriorityQueue(8 -> "rod", 8 -> "jane", 8 -> "freddie")

    "contain them all" in {
      val (a1, q1) = pq.dequeue
      val (a2, q2) = q1.dequeue
      val (a3, q3) = q2.dequeue

      Set(a1._2, a2._2, a3._2) must_== Set("rod", "jane", "freddie")
      q3 must_== PriorityQueue[Int, String]()
    }
  }
}
