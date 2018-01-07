package core.backend.common.algorithms


import core.utils._

import scalaz._, Scalaz._

/**
  * Created by Al on 30/12/2017.
  */
object FixedPointTraversal {
  def fixedPoint[E, A](searchStep: Set[A] => E \/ Set[A], initial: Set[(A, A)]): E \/ Set[(A, A)] = {
    // Memo is threaded through by the combinator
    def reachableFrom(root: A, memo: Map[A, Set[A]]): E \/ Set[A] = {
      var fringe: Set[A] = Set(root)
      var acc: Set[A] = Set(root)
      var okay: E \/ Unit = ().right

      while (fringe.nonEmpty && okay.isRight) {
        okay = for {
          found <- searchStep(fringe)
          memoized = found.filter(_ in memo)
          memoizedValues = memoized.flatMap(memo)
          _ = fringe = found.diff(acc).diff(memoized)
          _ = acc = acc.union(fringe).union(memoizedValues)
        } yield ()
      }

      okay.map(_ => acc)
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

  /**
    * Awkward, per-root implementation so we can reconstruct pairs afterwards. Otherwise would need a join
    * How about memoizing the search step using a suitable cache?
    */

  def upTo[E, A](
                  searchStep: Set[A] => E \/ Set[A],
                  initial: Set[A],
                  limit: Int
                ): E \/ Set[(A, A)] = {
    // do A depth first search up to a limit
    def fromRoot(root: A): E \/ Set[(A, A)] = {
      var count = limit
      var fringe: Set[A] = Set(root)
      var acc: Set[A] = Set(root)
      var okay: E \/ Unit = ().right
      while (count > 0 && okay.isRight) {
        count = count - 1
        okay = for {
          found <- searchStep(fringe)
          _ = fringe = found.diff(acc) // all those that haven't yet been found
          _ = acc = acc.union(fringe)
        } yield ()
      }
      okay.map(_ => acc.map((root, _)))
    }

    (for {
      root <- initial
    } yield fromRoot(root)).flattenE
  }

  private def doJoin[A](left: Set[(A, A)], right: Map[A, Set[A]]): Set[(A, A)] = {
    left.flatMap { case (from, middle) => right.getOrElse(middle, Set()).map((from, _))}
  }
}
