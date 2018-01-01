package core.backend.common.algorithms

import core.backend.common.EmptyFringeError
import core.utils._

import scala.collection.immutable.Queue
import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 31/12/2017.
  */
object PathFinding {
  def allShortestPathsImpl[E, A](start: Set[A], searchStep: A => E \/ Set[(A, A)], onEmptyFringe: => E): E \/ Set[Vector[A]] = {
    def aux(fringe: Queue[Vector[A]], alreadyExplored: Set[A], acc: Set[Vector[A]]): E \/ Set[Vector[A]] = {
      for {
        stepResult <- doStep(searchStep, fringe, alreadyExplored, onEmptyFringe)

        (newFringe, path, objects) = stepResult

        newExplored = alreadyExplored | objects
        newAcc = acc | objects.map(path :+ _)
        res <-
        if (newFringe.isEmpty)
          newAcc.right
        else
          aux(newFringe, newExplored, newAcc)
      } yield res
    }

    aux(toQueue(start.map(a => Vector[A](a))), Set(), Set()).recover{ case e if e == onEmptyFringe => Set()}
  }

  // Step function, fringe => NewFringe, pickedPath, newlyFound
  private def doStep[E, A](searchStep: A => E \/ Set[(A, A)], fringe: Queue[Vector[A]], alreadyExplored: Set[A], onEmptyFringe: => E): E \/ (Queue[Vector[A]], Vector[A], Set[A]) =
    if (fringe.nonEmpty) {
      val top = fringe.head // pop the top off of the fringe
      for {
        next <- searchStep(top.last)
        newObjects = next.mapProj2.diff(alreadyExplored)
        newFringe = fringe.tail ++ newObjects.diff(alreadyExplored).map(top :+ _) // todo: Probably slow
      } yield (newFringe, top, newObjects)
    } else {
      onEmptyFringe.left
    }

  private def toQueue[A](s: Set[A]): Queue[A] = Queue() ++ s


  def singleShortestsPathImpl[E, A](start: Set[A], end: A, searchStep: A => E \/ Set[(A, A)], onEmptyFringe: => E): E \/ Option[Vector[A]] = {
    def aux(fringe: Queue[Vector[A]], alreadyExplored: Set[A], acc: Set[Vector[A]]): E \/ Option[Vector[A]] = {
      for {
        stepResult <- doStep(searchStep, fringe, alreadyExplored, onEmptyFringe)
        (newFringe, path, objects) = stepResult
        res <-
          if (end in objects)  (path :+ end).some.right
          else {
            val newExplored = alreadyExplored | objects
            val newAcc = acc | objects.map(path :+ _)
            if (newFringe.isEmpty) None.right // return no result
            else aux(newFringe, newExplored, newAcc)
          }
      } yield res
    }

    aux(toQueue(start.map(a => Vector(a))), Set(), Set()).recover{ case EmptyFringeError => None}
  }  
}
