package impl.memory.methods

import core.backend.common.MissingTableName
import core.error.E
import core.intermediate.unsafe._
import impl.memory.{MemoryObject, MemoryTree, RelatedPair}
import core.utils._

import scalaz.Scalaz._
import scalaz._

trait SetImpl { self: ExecutorMethods with Joins with RepetitionImpl =>
  def findSingleSetImpl(t: UnsafeFindSingle, tree: MemoryTree): E \/ Set[MemoryObject] = {
    def recurse(t: UnsafeFindSingle) = findSingleSetImpl(t, tree)

    t match {
      case USFind(pattern) => tree.getOrError(pattern.tableName, MissingTableName(pattern.tableName)).flatMap(_.find(pattern).map(_.toSet))
      case USFrom(start, rel) => for {
        left <- recurse(start)
        res <- findPairsSetImpl(rel, left, tree).map(_.mapProj2)
      } yield res
      case USNarrowS(start, pattern) => for {
        broad <- recurse(start)
      } yield broad.filter(matches(_, pattern)) // todo: this should be more typesafe
    }
  }

  def findPairsSetImpl(t: UnsafeFindPair, left: Set[MemoryObject], tree: MemoryTree): E \/ Set[RelatedPair] = {
    def recurse(t: UnsafeFindPair, left: Set[MemoryObject]) = findPairsSetImpl(t, left, tree)
    t match {
      case USAnd(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- recurse(r, left)
      } yield {
        println("left = " + leftRes)
        println("right = " + rightRes)
        leftRes.intersect(rightRes)
      }

      case USAndSingle(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- findSingleSetImpl(r, tree)
      } yield leftRes.filter{case (a, b) => rightRes.contains(b)}

      case USOr(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- recurse(r, left)
      } yield leftRes.union(rightRes)

      case USChain(l, r) => for {
        lres <- recurse(l, left)
        rres <- recurse(r, lres.map(_._2))
      } yield {
        println("(Distinct) Chain Left result = " + lres)
        println("(Distinct) Chain right Result = " + rres)
        val res = joinSet(lres, rres)
        println("Chain res = " + res)
        res
      }

      case USDistinct(r) => for {
        rres <- recurse(r, left)
      } yield {
        println("Subexpr = " + r)
        println("Before distinction = " + rres)
        println("After distinction = " +  rres.filter{case (a, b) => a != b})
        rres.filter{case (a, b) => a != b}
      }

      case USId(_) => left.map(x => (x, x)).right

      case USNarrow(l, p) => for {
        broad <- recurse(l, left)
      } yield {
        println("(Distinct) Broad = " + broad)
        val res = broad.filter(pair => matches(pair._2, p))
        println("(Distinct) Narrowed = " + res)
        res
      }

      case USRel(rel) =>
        EitherOps.sequence(left.map {
          leftObject: MemoryObject => {
            val related = leftObject.getRelated(rel.name)
            val eRelatedObjects = EitherOps.sequence(related.map(o => tree.findObj(rel.to, o)))
            val res = eRelatedObjects.map(relatedObjects => relatedObjects.flatten.map((leftObject, _)))
            res
          }
        }).map(_.flatten)

      case USRevRel(rel) =>
        EitherOps.sequence(left.map {
          leftObject: MemoryObject => {
            val related = leftObject.getRevRelated(rel.name)
            val eRelatedObjects = EitherOps.sequence(related.map(o => tree.findObj(rel.from, o)))
            val res = eRelatedObjects.map(relatedObjects => relatedObjects.flatten.map((leftObject, _)))
            res
          }
        }).map(_.flatten)

      case USUpto(n, rel) =>
        val stepFunction: Set[MemoryObject] => E \/ Set[MemoryObject] = left => findPairsSetImpl(rel, left, tree).map(_.mapProj2)
        upTo(stepFunction, left, n)

      case USBetween(low, high, rel) => recurse(USChain(USExactly(low, rel), USUpto(high - low, rel)), left)
      case USAtleast(n, rel) =>
        if (n > 0) {
          recurse(USChain(USExactly(n, rel), USAtleast(0, rel)), left)
        } else {
          // otherwise find a fixed point
          println("Atleast: Left = " + left.mkString("\n\t\t\t"))
          val stepFunction: Set[MemoryObject] => E \/ Set[MemoryObject] = left => findPairsSetImpl(rel, left, tree).map(_.mapProj2)
          for {
            res <- fixedPoint(stepFunction, left.map(x => (x, x)))
          } yield res
        }
      case USExactly(n, rel) => if (n <= 0) {
        left.map(x => (x, x)).right
      } else {
        recurse(USChain(rel, USExactly(n-1, rel)), left) // todo: this is probably quite slow
      }
    }
  }
}