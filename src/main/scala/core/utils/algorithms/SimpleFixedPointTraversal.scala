package core.utils.algorithms


import core.utils._

import scala.collection.mutable
import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 30/12/2017.
  *
  * Contains a number of utility methods for implementing transitive relation methods in the LMDB and Memory implementations,
  * where we only look at the rhs of a relation
  */

object SimpleFixedPointTraversal {
  /**
    * Find the transitive closure of the search step relation
    * @param searchStep - a collective relation following function
    * @param root - A set of pairs found to the left of the fixed point relation being searched
    * @tparam E - type of errors that may be thrown
    * @tparam A - type of objects used
    * @return
    */
  def fixedPoint[E, A](searchStep: A => E \/ Set[A], root: A): E \/ scala.collection.immutable.Set[A] = {
    val fringe: mutable.Queue[A] = new mutable.Queue()
    fringe.enqueue(root)
    val acc: mutable.Set[A] = mutable.Set[A](root)
    var okay: E \/ Unit = ().right

    while (fringe.nonEmpty && okay.isRight) {
      okay = for {
        found <- searchStep(fringe.dequeue())
        newIds = found.diff(acc)
        _ = fringe ++= newIds
        _ = acc ++= newIds
      } yield ()
    }
    okay.map(_ => acc.toSet)
  }


  /**
    *
    * @param searchStep - search step to repeat upto limit times
    * @param root - initial value to search from
    * @param limit - number of repetitions allowed
    * @tparam E - Errors that may be thrown
    * @tparam A - Search node
    * @return
    */

  def upTo[E, A](
                  searchStep: A => E \/ Set[A],
                  root: A,
                  limit: Int
                ): E \/ scala.collection.immutable.Set[A] = {
    // do A depth first search up to a limit

      var count = limit
      var fringe: Set[A] = Set(root)
      val acc: mutable.Set[A] = mutable.Set[A](root)
      var okay: E \/ Unit = ().right
      while (count > 0 && okay.isRight) {
        count = count - 1
        okay = for {
          found <- fringe.map(searchStep).flattenE
          _ = fringe = found.diff(acc) // all those that haven't yet been found
          _ = acc ++= fringe
        } yield ()
      }
      okay.map(_ => acc.toSet)
  }
}
