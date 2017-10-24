package db.memory

import core.RelationAttributes
import core.concrete.relations.CompletedRelation
import core.containers.{Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate._
import db.common.{DBObject, MissingTableName}
import db.interfaces.{DBExecutor, Extractor}
import schema.{Findable, RelationName, SchemaObject, TableName}
import view.View

import utils._

import scala.concurrent.ExecutionContext
import scalaz.Scalaz._
import scalaz._


/**
  * Created by Al on 22/10/2017.
  */
class InMemoryExecutor extends DBExecutor {
  override def findAll[A](t: FindSingle[A])(implicit e: ExecutionContext, extractor: Extractor[A]): Operation[E, Vector[A]] = readOp(findSingle(t: FindSingle[Any]).flatMap(extractor.fromRow))

  override def findAllPairs[A, B](t: FindPair[A, B])(implicit e: ExecutionContext, ea: Extractor[A], eb: Extractor[B]): Operation[E, Vector[(A, B)]] = readOp(???)

  override def findDistinct[A](t: FindSingle[A])(implicit e: ExecutionContext): Operation[E, Set[A]] = ???

  override def findDistinctPairs[A, B](t: FindPair[A, B])(implicit e: ExecutionContext, ea: Extractor[A], eb: Extractor[B]): Operation[E, Vector[(A, B)]] = ???

  override def shortestPath[A](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext, sa: SchemaObject[A]): Operation[E, Option[Path[A]]] = ???

  override def allShortestPaths[A](start: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext, sa: SchemaObject[A]): Operation[E, Set[Path[A]]] = ???

  override def insert[A, B](t: TraversableOnce[CompletedRelation[A, B]])(implicit sa: SchemaObject[A], sb: SchemaObject[B]): Operation[E, Unit] = ???

  type MemoryTree = Map[TableName, MemoryTable]
  type RelatedPair = (MemoryObject, MemoryObject)

  sealed trait MemoryTable {
    def objects: Vector[MemoryObject]
  }

  sealed trait MemoryObject {
    def relations: Map[RelationName, Vector[MemoryObject]]
    def revRelations: Map[RelationName, Vector[MemoryObject]]
    def value: Vector[DBObject]
  }

  val relationNames: Map[RelationAttributes[Any, Any], RelationName] = ???


  var memoryStore: Map[View, MemoryTree] = Map()



  def matches[A](o: MemoryObject, p: Findable[A]): Boolean = ??? // check that the object matches the pattern


  // Todo: need to kill off this Parameter

  def findSingle(t: FindSingle[Any])(tree: MemoryTree): E \/ Vector[MemoryObject] = {
    def recurse(t: FindSingle[Any]) = findSingle(t)(tree)

    t match {
      case Find(pattern) =>
        for {
          broad <- tree.get(pattern.tableName).map(_.objects).fold(\/.left[E, Vector[MemoryObject]](MissingTableName(pattern.tableName)))(_.right[E])
        } yield broad.filter(matches(_, pattern))
      case From(start, rel) => for {
        left <- recurse(start)
        res <- findPairs(rel, left)(tree).map(v => v.map(_._2))
      } yield res
      case NarrowS(start, pattern) => for {
        broad <- recurse(start)
      } yield broad.filter(matches(_, pattern)) // todo: this should be more typesafe
    }
  }

  def findPairs(t: FindPair[Any, Any], left: Vector[MemoryObject])(tree: MemoryTree): E \/ Vector[RelatedPair] = {
    def recurse(t: FindPair[Any, Any], left: Vector[MemoryObject]) = findPairs(t, left)(tree)

    t match {
      case And(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- recurse(r, left)
      } yield leftRes.intersect(rightRes)
      case Or(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- recurse(r, left)
      } yield leftRes.union(rightRes)

      case Chain(l, r) => for {
        lres <- recurse(l, left)
        rres <- recurse(r, lres.map(_._2))
      } yield join(lres, rres)

      case Id() => left.map(x => (x, x)).right

      case Narrow(l, p) => for {
        broad <- recurse(l, left)
      } yield broad.filter(pair => matches(pair._2, p))

      case Rel(rel) => \/-(left.flatMap(o => o.relations.getOrElse(rel.name, Vector()).map((o, _))))

      case RevRel(rel) => \/-(left.flatMap(o => o.revRelations.getOrElse(rel.name, Vector()).map((o, _))))

      case Upto(n, rel) =>
        if (n <= 0) left.map(x => (x, x)).right
        else for {
          lres <- recurse(rel, left)
          rres <- fixedPoint(left => findPairsSet(rel, left)(tree), lres.mapProj2.toSet.map(x => (x, x)), Number(n))
        } yield join(lres, rres.toVector)
      case Between(low, high, rel) => recurse(Chain(Exactly(low, rel), Upto(high - low, rel)), left)
      case AtLeast(n, rel) =>
        if (n > 0) {
          recurse(Chain(Exactly(n, rel), AtLeast(0, rel)), left)
        } else {
          // otherwise find a fixed point
          for {
            res <- fixedPoint(left => findPairsSet(rel, left)(tree), left.toSet.map(x => (x, x)), NoLimit)
          } yield res.toVector
        }
      case Exactly(n, rel) => if (n <= 0) {
        left.map(x => (x, x)).right
      } else {
        recurse(Chain(rel, Exactly(n-1, rel)), left) // todo: this is probably quite slow
      }
    }
  }

  def findPairsSet(t: FindPair[Any, Any], left: Set[MemoryObject])(tree: MemoryTree): E \/ Set[RelatedPair] = {
    def recurse(t: FindPair[Any, Any], left: Set[MemoryObject]) = findPairsSet(t, left)(tree)
    t match {
      case And(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- recurse(r, left)
      } yield leftRes.intersect(rightRes)
      case Or(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- recurse(r, left)
      } yield leftRes.union(rightRes)

      case Chain(l, r) => for {
        lres <- recurse(l, left)
        rres <- recurse(r, lres.map(_._2))
      } yield joinSet(lres, rres)

      case Id() => left.map(x => (x, x)).right

      case Narrow(l, p) => for {
        broad <- recurse(l, left)
      } yield broad.filter(pair => matches(pair._2, p))

      case Rel(rel) => \/-(left.flatMap(o => o.relations.getOrElse(rel.name, Vector()).map((o, _))))

      case RevRel(rel) => \/-(left.flatMap(o => o.revRelations.getOrElse(rel.name, Vector()).map((o, _))))

      case Upto(n, rel) =>
        if (n <= 0) left.map(x => (x, x)).right
        else for {
          lres <- recurse(rel, left)
          rres <- fixedPoint(left => findPairsSet(rel, left)(tree), lres.mapProj2.map(x => (x, x)), Number(n))
        } yield joinSet(lres, rres)
      case Between(low, high, rel) => recurse(Chain(Exactly(low, rel), Upto(high - low, rel)), left)
      case AtLeast(n, rel) =>
        if (n > 0) {
          recurse(Chain(Exactly(n, rel), AtLeast(0, rel)), left)
        } else {
          // otherwise find a fixed point
          for {
            res <- fixedPoint(left => findPairsSet(rel, left)(tree), left.map(x => (x, x)), NoLimit)
          } yield res
        }
      case Exactly(n, rel) => if (n <= 0) {
        left.map(x => (x, x)).right
      } else {
        recurse(Chain(rel, Exactly(n-1, rel)), left) // todo: this is probably quite slow
      }
    }
  }

  def fixedPoint(searchStep: (Set[MemoryObject]) => E \/ Set[RelatedPair], initial: Set[RelatedPair], limit: Limit): E \/ Set[RelatedPair] = {
    def aux(pairsToExplore: Set[RelatedPair], alreadyExplored: Set[MemoryObject], acc: Set[RelatedPair], limit: Limit): E \/ Set[RelatedPair] = for {
        foundPairs <- searchStep(pairsToExplore)
        newPairs = foundPairs.filter {case (_, r) => !alreadyExplored.contains(r)}
        newAcc = acc | joinSet(acc, newPairs)
        res <- limit match {
          case Number(n) => if (newPairs.isEmpty || n <= 1) newAcc.right else aux(newPairs, alreadyExplored | newPairs.map(_._1), newAcc, Number(n-1))
          case NoLimit => if (newPairs.isEmpty) newAcc.right else aux(newPairs, alreadyExplored | newPairs.map(_._1), newAcc, NoLimit)
        }
      } yield res

    limit match {
      case Number(0) => initial.right
      case _ => aux(initial, initial.mapProj1, initial, limit)
    }
  }

  // slow join

  def join(leftRes: Vector[RelatedPair], rightRes: Vector[RelatedPair]): Vector[RelatedPair] =
    for {
      (from, to) <- leftRes
      right <- rightRes.collect {case (f, t) if f == to => t}
    } yield (from, right)

  def joinSet(leftRes: Set[RelatedPair], rightRes: Set[RelatedPair]): Set[RelatedPair] =
    for {
      (from, to) <- leftRes
      right <- rightRes.collect {case (f, t) if f == to => t}
    } yield (from, right)


  def readOp[A](f: MemoryTree => E \/ A): Operation[E, A] = ???


  sealed trait Limit
  case object NoLimit extends Limit
  case class Number(n: Int) extends Limit
}
