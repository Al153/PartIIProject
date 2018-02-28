package core.utils.algorithms

import core.utils._

import scala.collection.immutable.Queue
import scala.collection.mutable
import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 31/12/2017.
  *
  * Suite of pathfinding methods for the LMDB and memory implementations
  * Non recursive implementations to avoid stack overflows
  */
object PathFinding {
  /**
    *
    * @param start - Starting set
    * @param searchStep - step to use to search
    * @tparam E - type of errors
    * @tparam A -  types of nodes
    * @return shortest path to each reachable node
    */

  def allShortestPathsImpl[E, A](start: Set[A], searchStep: A => E \/ Set[A]): E \/ Set[List[A]] = {
    var fringe: Queue[List[A]] = toQueue(start.map(a => List[A](a)))
    var alreadyExplored: Set[A] = Set()
    var resBuilder:mutable.Builder[List[A], Set[List[A]]] = Set.newBuilder[List[A]]

    var okay: E \/ Unit = ().right

    while (fringe.nonEmpty && okay.isRight) {
      okay = for {
        stepResult <- doStep(searchStep, fringe, alreadyExplored)
        (newFringe, path, objects) = stepResult


        _ =  resBuilder ++= objects.map(_ :: path)
        _ = fringe = newFringe
        _ = alreadyExplored = alreadyExplored | objects

      } yield ()
    }

    okay.map(_ => resBuilder.result())

  }

  // Step function, fringe => NewFringe, pickedPath, newlyFound
  private def doStep[E, A](searchStep: A => E \/ Set[A], fringe: Queue[List[A]], alreadyExplored: Set[A]): E \/ (Queue[List[A]], List[A], Set[A]) =
    if (fringe.nonEmpty) {
      val top = fringe.head // pop the top off of the fringe
      for {
        next <- searchStep(top.head)
        newObjects = next.diff(alreadyExplored)
        newFringe = fringe.tail ++ newObjects.diff(alreadyExplored).map(_ :: top)
      } yield (newFringe, top, newObjects)
    } else {
      (fringe, List(), alreadyExplored).right
    }

  private def toQueue[A](s: Set[A]): Queue[A] = Queue() ++ s

  /**
    *
    * @param start - Starting set
    * @param end - target node
    * @param searchStep - step to use to search
    * @tparam E - type of errors
    * @tparam A -  types of nodes
    * @return shortest path to the end
    */
  def singleShortestsPathImpl[E, A](start: Set[A], end: A, searchStep: A => E \/ Set[A]): E \/ Option[List[A]] = {
    var fringe: Queue[List[A]] = toQueue(start.map(a => List[A](a)))
    var alreadyExplored: Set[A] = Set()
    var result: E \/ Option[List[A]] = None.right
    var done = false

    while (fringe.nonEmpty && result.isRight && !done) {
      result = for {
        stepResult <- doStep(searchStep, fringe, alreadyExplored)
        (newFringe, path, objects) = stepResult
        r = if (end in objects) {
          done = true
          (end :: path).some
        } else {
          fringe = newFringe
          alreadyExplored =  alreadyExplored | objects
          None
        }
      } yield r
    }
    result
  }  
}
