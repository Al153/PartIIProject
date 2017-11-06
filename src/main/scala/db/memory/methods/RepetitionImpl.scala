package db.memory.methods

import core.error.E
import db.common.{Limit, Number}
import db.memory.{MemoryObject, RelatedPair}

import scalaz._
import Scalaz._
import utils._

import scalaz.\/

trait RepetitionImpl { self: ExecutorMethods with Joins =>
  /**
    *   Find the fixed point of the search step function
    */

  // TODO: this is incorrect - assumes no loops, etc
  // need separate fixed point and non-fixed point algos

  protected def fixedPoint(searchStep: Set[MemoryObject] => E \/ Set[RelatedPair], initial: Set[RelatedPair]): E \/ Set[RelatedPair] = {

    /* Algorithm Idea:
     * - Step 1: Find a set of all reachable pairs: Iteratively search for all linked pairs
     *
     * - Step 2: Find transitive closure of all pairs
     */

    def findPairs(fringe: Set[MemoryObject], alreadyExplored: Set[MemoryObject], acc: Set[RelatedPair]): E \/ Set[RelatedPair] =
      for {
        foundPairs <- searchStep(fringe)
        newExplored = alreadyExplored | fringe
        newRight = foundPairs.mapProj2.diff(newExplored)
        newAcc = acc | foundPairs
        res <-
          if (newRight.isEmpty)
            newAcc.right
          else {
            findPairs(newRight, newExplored, newAcc)
          }
      } yield res

    val subGraph = findPairs(initial.mapProj2, Set(), initial)

    ??? // next step is to do a mega join over these pairs starting from the initial set.

    def traverseSubgraph()
  }


  protected def upTo(
                      searchStep: MemoryObject => E \/ Set[MemoryObject],
                      initial: Set[MemoryObject],
                      limit: Int): E \/ Set[RelatedPair] = {
    // do A depth first search up to a limit

    def fromRoot(root: MemoryObject, limit: Int): E \/ Set[RelatedPair] = {
      def aux(limit: Int, fringe: Set[MemoryObject], acc: Set[MemoryObject]): E \/ Set[MemoryObject] = {
        if (limit <= 0) acc.right
        else for {
          newFringe <- fringe.map(searchStep).flattenE
          newAcc = acc.union(newFringe)
          res <- aux(limit - 1, newFringe, newAcc)
        } yield res
      }

      aux(limit, Set(root), Set(root)).map(s => s.map((root, _)))
    }


    (for {
      root <- initial
    } yield fromRoot(root, limit)).flattenE

  }
}