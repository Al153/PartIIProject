package impl.memory.methods

import core.error.E
import impl.memory.{MemoryObject, RelatedPair}
import core.utils._

import scalaz.Scalaz._
import scalaz.\/

trait RepetitionImpl { self: ExecutorMethods with Joins =>
  /**
    *   Find the fixed point of the search step function
    */

  protected def fixedPoint(searchStep: Set[MemoryObject] => E \/ Set[MemoryObject], initial: Set[RelatedPair]): E \/ Set[RelatedPair] = {
    println("Finding fixed points")

    def reachableFrom(root: MemoryObject, memo: Map[MemoryObject, Set[MemoryObject]]): E \/ Set[MemoryObject] = {
      // fromMemo: found -> keysFoundInMemo, ValuesFromMemo
      def fromMemo(found: Set[MemoryObject]): (Set[MemoryObject], Set[MemoryObject]) = {
        val keys = found.filter(_ in memo)
        val values = keys.flatMap(memo)
        (keys, values)
      }

      def aux(fringe: Set[MemoryObject], acc: Set[MemoryObject]): E \/ Set[MemoryObject] = {
        if (fringe.isEmpty) acc.right
        else for {
          found <- searchStep(fringe)
          (memoized, memoizedValues) = fromMemo(found)
          newFringe = found.diff(acc).diff(memoized)
          newAcc = acc.union(newFringe).union(memoizedValues)
          res <- aux(newFringe, newAcc)
        } yield res
      }

      aux(Set(root), Set(root))
    }

    def combinator(root: MemoryObject, eacc: E \/ Map[MemoryObject, Set[MemoryObject]]): E \/ Map[MemoryObject, Set[MemoryObject]] = {
      for {
        acc <- eacc
        reachable <- reachableFrom(root, acc)
      } yield acc + (root -> reachable)
    }

    val allPairs = initial.mapProj2.foldRight(Map[MemoryObject, Set[MemoryObject]]().right[E])(combinator)

    for {
      dict <- allPairs
    } yield doJoin(initial, dict)
  }


  protected def upTo(
                      searchStep: Set[MemoryObject] => E \/ Set[MemoryObject],
                      initial: Set[MemoryObject],
                      limit: Int): E \/ Set[RelatedPair] = {
    // do A depth first search up to a limit

    def fromRoot(root: MemoryObject, limit: Int): E \/ Set[RelatedPair] = {
      def aux(limit: Int, fringe: Set[MemoryObject], acc: Set[MemoryObject]): E \/ Set[MemoryObject] = {
        if (limit <= 0 || fringe.isEmpty) acc.right
        else for {
          found <- searchStep(fringe)
          newFringe = found.diff(acc) // all those that haven't yet been found
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

  private def doJoin(left: Set[RelatedPair], right: Map[MemoryObject, Set[MemoryObject]]): Set[RelatedPair] = {
    left.flatMap { case (from, middle) => right.getOrElse(middle, Set()).map((from, _))}
  }
}