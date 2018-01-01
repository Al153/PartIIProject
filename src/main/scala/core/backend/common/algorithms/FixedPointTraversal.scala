package core.backend.common.algorithms


import core.utils._

import scalaz._, Scalaz._

/**
  * Created by Al on 30/12/2017.
  */
object FixedPointTraversal {
  def fixedPoint[E, A](searchStep: Set[A] => E \/ Set[A], initial: Set[(A, A)]): E \/ Set[(A, A)] = {
    def reachableFrom(root: A, memo: Map[A, Set[A]]): E \/ Set[A] = {
      // fromMemo: found -> keysFoundInMemo, ValuesFromMemo
      def fromMemo(found: Set[A]): (Set[A], Set[A]) = {
        val keys = found.filter(_ in memo)
        val values = keys.flatMap(memo)
        (keys, values)
      }

      def aux(fringe: Set[A], acc: Set[A]): E \/ Set[A] = {
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

    def combinator(root: A, eacc: E \/ Map[A, Set[A]]): E \/ Map[A, Set[A]] = {
      for {
        acc <- eacc
        reachable <- reachableFrom(root, acc)
      } yield acc + (root -> reachable)
    }

    val allPairs = initial.mapProj2.foldRight(Map[A, Set[A]]().right[E])(combinator)

    for {
      dict <- allPairs
    } yield doJoin(initial, dict)
  }


  def upTo[E, A](
                  searchStep: Set[A] => E \/ Set[A],
                  initial: Set[A],
                  limit: Int
                ): E \/ Set[(A, A)] = {
    // do A depth first search up to a limit

    def fromRoot(root: A, limit: Int): E \/ Set[(A, A)] = {
      def aux(limit: Int, fringe: Set[A], acc: Set[A]): E \/ Set[A] = {
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

  private def doJoin[A](left: Set[(A, A)], right: Map[A, Set[A]]): Set[(A, A)] = {
    left.flatMap { case (from, middle) => right.getOrElse(middle, Set()).map((from, _))}
  }
}
