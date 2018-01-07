package core.backend.common.algorithms

import core.backend.common.algorithms.PathFinding.doStep
import core.utils._

import scala.collection.immutable.Queue
import scala.collection.mutable
import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 31/12/2017.
  */
object PathFinding {

  // Non recursive implementations to avoid stack overflows
  def allShortestPathsImpl[E, A](start: Set[A], searchStep: A => E \/ Set[(A, A)]): E \/ Set[Vector[A]] = {
    var fringe: Queue[Vector[A]] = toQueue(start.map(a => Vector[A](a)))
    var alreadyExplored: Set[A] = Set()
    var resBuilder:mutable.Builder[Vector[A], Set[Vector[A]]] = Set.newBuilder[Vector[A]]

    var okay: E \/ Unit = ().right

    while (fringe.nonEmpty && okay.isRight) {
      okay = for {
        stepResult <- doStep(searchStep, fringe, alreadyExplored)
        (newFringe, path, objects) = stepResult


        _ =  resBuilder ++= objects.map(path :+ _)
        _ = fringe = newFringe
        _ = alreadyExplored = alreadyExplored | objects

      } yield ()
    }

    okay.map(_ => resBuilder.result())

  }

  // Step function, fringe => NewFringe, pickedPath, newlyFound
  private def doStep[E, A](searchStep: A => E \/ Set[(A, A)], fringe: Queue[Vector[A]], alreadyExplored: Set[A]): E \/ (Queue[Vector[A]], Vector[A], Set[A]) =
    if (fringe.nonEmpty) {
      val top = fringe.head // pop the top off of the fringe
      for {
        next <- searchStep(top.last)
        newObjects = next.mapProj2.diff(alreadyExplored)
        newFringe = fringe.tail ++ newObjects.diff(alreadyExplored).map(top :+ _) // todo: Probably slow
      } yield (newFringe, top, newObjects)
    } else {
      (fringe, Vector(), alreadyExplored).right
    }

  private def toQueue[A](s: Set[A]): Queue[A] = Queue() ++ s


  def singleShortestsPathImpl[E, A](start: Set[A], end: A, searchStep: A => E \/ Set[(A, A)]): E \/ Option[Vector[A]] = {
    var fringe: Queue[Vector[A]] = toQueue(start.map(a => Vector[A](a)))
    var alreadyExplored: Set[A] = Set()
    var result: E \/ Option[Vector[A]] = None.right
    var done = false

    while (fringe.nonEmpty && result.isRight && !done) {
      result = for {
        stepResult <- doStep(searchStep, fringe, alreadyExplored)
        (newFringe, path, objects) = stepResult

        r = if (end in objects) {
          done = true
          (path :+ end).some
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
