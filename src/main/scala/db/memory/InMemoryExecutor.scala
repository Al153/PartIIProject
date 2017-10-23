package db.memory

import core.RelationAttributes
import core.concrete.relations.CompletedRelation
import core.containers.{ConstrainedFuture, Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate._
import db.common.{DBObject, MissingTableName, MissingViewError}
import db.interfaces.{DBExecutor, Extractor}
import schema.{Findable, RelationName, SchemaObject, TableName}
import view.View

import scala.concurrent.ExecutionContext
import scalaz._
import Scalaz._

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

  sealed trait MemoryTable {
    def objects: Vector[MemoryObject]
  }

  sealed trait MemoryObject {
    def relations: Map[RelationName, Vector[MemoryObject]]
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

  def findPairs(t: FindPair[Any, Any], left: Vector[MemoryObject])(tree: MemoryTree): E \/ Vector[(MemoryObject, MemoryObject)] = {
    def recurse(t: FindPair[Any, Any], left: Vector[MemoryObject]) = findPairs(t, left)(tree)

    t match {
      case And(l, r) =>
      case Or(l, r) =>
      case Chain(l, r) => for {
        lres <- recurse(l, left)
        rres <- recu
      }
      case Id() => left.map(x => (x, x)).right
      case Narrow(l, p) => for {
        broad <- recurse(l, left)
      } yield broad.filter(pair => matches(pair._2, p))
      case Rel(rel) =>
      case RevRel(rel) =>

      case Upto(n, rel) => ???
      case Between(low, high, rel) => ???
      case AtLeast(n, rel) => ???
      case Exactly(n, rel) => ???


    }
  }


  def readOp[A](f: MemoryTree => E \/ A): Operation[E, A] = ???
}
