package impl.memory.methods

import core.backend.common.MissingTableName
import core.error.E
import core.intermediate.Find
import core.intermediate.unsafe._
import core.schema.{SchemaDescription, SchemaObject}
import impl.memory.{MemoryObject, MemoryTree}
import impl.memory.RelatedPair
import core.utils._

import scalaz.Scalaz._
import scalaz._

trait VectorImpl { self: ExecutorMethods with SetImpl with Joins with RepetitionImpl =>
  def find[A](a: A, t: MemoryTree)(implicit sa: SchemaObject[A], sd: SchemaDescription): E \/ Set[MemoryObject] =
    for {
      unsafeQuery <- Find(sa.findable(a)).getUnsafe
      res <- findSingleImpl(unsafeQuery, t)
    } yield res.toSet

  def findSingleImpl(t: UnsafeFindSingle, tree: MemoryTree): E \/ Vector[MemoryObject] = {
    def recurse(t: UnsafeFindSingle) = findSingleImpl(t, tree)

    t match {
      case USFind(pattern) => tree.getOrError(pattern.tableName, MissingTableName(pattern.tableName)).flatMap(_.find(pattern))
      case USFrom(start, rel) => for {
        left <- recurse(start)
        res <- findPairsImpl(rel, left, tree).map(_.mapProj2)
      } yield res
      case USNarrowS(start, pattern) => for {
        broad <- recurse(start)
      } yield broad.filter(matches(_, pattern)) // todo: this should be more typesafe
    }
  }


  def findPairsImpl(q: UnsafeFindPair, left: Vector[MemoryObject], tree: MemoryTree): E \/ Vector[RelatedPair] = {
    println(tree)
    def recurse(t: UnsafeFindPair, left: Vector[MemoryObject]) = findPairsImpl(t, left, tree)

    q match {
      case USAnd(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- recurse(r, left)
      } yield leftRes.intersect(rightRes)

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
        rres <- recurse(r, lres.mapProj2.distinct) // reduce double joining
      } yield join(lres, rres)


      case USDistinct(r) => for {
        rres <- recurse(r, left)
      } yield rres.filter{case (a, b) => a != b}


      case USId(_) => left.map(x => (x, x)).right

      case USNarrow(l, p) => for {
        broad <- recurse(l, left)
      } yield broad.filter(pair => matches(pair._2, p))

      case USRel(rel) =>
        EitherOps.sequence(left.map {
          leftObject: MemoryObject => {
            val related = leftObject.getRelated(rel.name).toVector
            val eRelatedObjects = EitherOps.sequence(related.map(o => tree.findObj(rel.to, o)))
            val res = eRelatedObjects.map(relatedObjects => relatedObjects.flatten.map((leftObject, _)))
            res
          }
        }).map(_.flatten)


      case USRevRel(rel) =>
        println("RevRel Left = " + left)
        EitherOps.sequence(left.map {
          leftObject: MemoryObject => {
            val related = leftObject.getRevRelated(rel.name).toVector
            println("Rev related = " + related.mkString("\n"))
            val eRelatedObjects = EitherOps.sequence(related.map(o => tree.findObj(rel.from, o)))
            val res = eRelatedObjects.map(relatedObjects => relatedObjects.flatten.map((leftObject, _)))
            res
          }
        }).map(_.flatten)

      case USUpto(n, rel) =>
        val stepFunction: Set[MemoryObject] => E \/ Set[MemoryObject] = left => findPairsSetImpl(rel, left, tree).map(_.mapProj2)
          for {
            rres <- upTo(stepFunction, left.toSet, n)
          } yield rres.toVector

      case USBetween(low, high, rel) => {
        recurse(USChain(USExactly(low, rel), USUpto(high - low, rel)), left)
      }
      case USAtleast(n, rel) =>
        if (n > 0) {
          recurse(USChain(USExactly(n, rel), USAtleast(0, rel)), left)
        } else {
          // otherwise find a fixed point
          println("Atleast: Finding fixed point")
          println("Atleast: Left = " + left.mkString("\n\t\t\t"))
          val stepFunction: Set[MemoryObject] => E \/ Set[MemoryObject] = left => findPairsSetImpl(rel, left, tree).map(_.mapProj2)
          for {
            res <- fixedPoint(stepFunction, left.toSet.mapPair)
          } yield res.toVector
        }
      case USExactly(n, rel) => if (n <= 0) {
        left.map(x => (x, x)).right
      } else {
        recurse(USChain(rel, USExactly(n-1, rel)), left) // todo: this is probably quite slow
      }
    }
  }
}