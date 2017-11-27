package impl.memory.methods

import core.error.E
import core.intermediate.unsafe.UnsafeFindable
import core.backend.common.EmptyFringeError
import impl.memory.{MemoryPath, RelatedPair}

import scala.collection.immutable.Queue
import scalaz._
import Scalaz._
import core.utils._
import impl.memory.{MemoryObject, MemoryPath}

trait PathFinding { self: ExecutorMethods =>
  // Breadth first == dijkstra's

  def allShortestPathsImpl(start: Set[MemoryObject], searchStep: MemoryObject => E \/ Set[RelatedPair]): E \/ Set[MemoryPath] = {
    def aux(fringe: Queue[MemoryPath], alreadyExplored: Set[MemoryObject], acc: Set[MemoryPath]): E \/ Set[MemoryPath] = {
      for {
        stepResult <- doStep(searchStep, fringe, alreadyExplored)

        (newFringe, path, objects) = stepResult

        newExplored = alreadyExplored | objects
        newAcc = acc | objects.map(path + _)
        res <-
        if (newFringe.isEmpty)
          newAcc.right
        else
          aux(newFringe, newExplored, newAcc)
      } yield res
    }

    aux(toQueue(start.map(MemoryPath.apply)), Set(), Set()).recover{ case EmptyFringeError => Set()}
  }

  // Step function, fringe => NewFringe, pickedPath, newlyFound
  private def doStep(searchStep: MemoryObject => E \/ Set[RelatedPair], fringe: Queue[MemoryPath], alreadyExplored: Set[MemoryObject]): E \/ (Queue[MemoryPath], MemoryPath, Set[MemoryObject]) =
    if (fringe.nonEmpty) {
      val top = fringe.head // pop the top off of the fringe
      for {
        next <- searchStep(top.getLast)
        newObjects = next.mapProj2.diff(alreadyExplored)
        newFringe = fringe.tail ++ newObjects.diff(alreadyExplored).map(top + _) // todo: Probably slow
      } yield (newFringe, top, newObjects)
    } else {
      EmptyFringeError.left
    }

  private def toQueue[A](s: Set[A]): Queue[A] = Queue() ++ s


  def singleShortestsPathImpl(start: Set[MemoryObject], end: UnsafeFindable, searchStep: MemoryObject => E \/ Set[RelatedPair]): E \/ Option[MemoryPath] = {
    def aux(fringe: Queue[MemoryPath], alreadyExplored: Set[MemoryObject], acc: Set[MemoryPath]): E \/ Option[MemoryPath] = {
      for {
        stepResult <- doStep(searchStep, fringe, alreadyExplored)
        (newFringe, path, objects) = stepResult
        res <- objects.find(matches(_, end)) match {
          case None =>
            val newExplored = alreadyExplored | objects
            val newAcc = acc | objects.map(path + _)
            if (newFringe.isEmpty) None.right // return no result
            else aux(newFringe, newExplored, newAcc)
          case Some(o) => (path + o).some.right
        }
      } yield res
    }

    aux(toQueue(start.map(MemoryPath.apply)), Set(), Set()).recover{ case EmptyFringeError => None}
  }
}