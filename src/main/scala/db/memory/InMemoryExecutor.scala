package db.memory

import core.concrete.relations.CompletedRelation
import core.containers.{Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate._
import db.interfaces.{DBExecutor, Extractor}
import schema.SchemaObject
import utils._

import scala.concurrent.ExecutionContext
import scalaz.Scalaz._
import scalaz._


/**
  * Created by Al on 22/10/2017.
  */
class InMemoryExecutor(instance: MemoryInstance) extends DBExecutor {
  override def findAll[A](q: FindSingle[A])(implicit e: ExecutionContext, ea: Extractor[A]): Operation[E, Vector[A]] =
    instance.readOp(
      t =>
        for {
          v <- findSingleImpl(q.getUnsafe, t)
          res <- EitherOps.sequence(v.map(o => ea.fromRow(o.value)))
        } yield res
    )

  override def findAllPairs[A, B](q: FindPair[A, B])(implicit e: ExecutionContext, ea: Extractor[A], eb: Extractor[B]): Operation[E, Vector[(A, B)]] =
    instance.readOp {
      t =>
        for {
          initial <- findSingleImpl(Find(q.sa.generalPattern)(q.sa).getUnsafe, t)
          v <- findPairsImpl(q.getUnsafe, initial, t)
          res <- EitherOps.sequence(
            v.map {
              case (l, r) =>
                for {
                  a <- ea.fromRow(l.value)
                  b <- eb.fromRow(r.value)
                } yield (a, b)
            })
        } yield res
    }

  override def findDistinct[A](q: FindSingle[A])(implicit e: ExecutionContext, ea: Extractor[A]): Operation[E, Set[A]] =
    instance.readOp(
      t =>
        for {
          v <- findSingleSetImpl(q.getUnsafe, t)
          res <- EitherOps.sequence(v.map(o => ea.fromRow(o.value)))
        } yield res
    )

  override def findDistinctPairs[A, B](q: FindPair[A, B])(implicit e: ExecutionContext, ea: Extractor[A], eb: Extractor[B]): Operation[E, Set[(A, B)]] =
    instance.readOp {
      t =>
        for {
          initial <- findSingleSetImpl(Find(q.sa.generalPattern)(q.sa).getUnsafe, t)
          v <- findPairsSetImpl(q.getUnsafe, initial, t)
          res <- EitherOps.sequence(
            v.map {
              case (l, r) =>
                for {
                  a <- ea.fromRow(l.value)
                  b <- eb.fromRow(r.value)
                } yield (a, b)
            })
        } yield res
    }

  override def shortestPath[A](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext, sa: SchemaObject[A]): Operation[E, Option[Path[A]]] =
    instance.readOp {
      tree =>
        for {
          initial <- find(start, tree)
          erasedRes <- singleShortestsPathImpl(initial, sa.findable(end).getUnsafe, o => findPairsSetImpl(relationalQuery.tree.getUnsafe, Set(o), tree))
          res <-  erasedRes.map(r => Path.from(r.toErasedPath, sa)).fold(Option.empty[Path[A]].right[E])(_.map(_.some))
        } yield res // todo: make this properly return an option
    }

  override def allShortestPaths[A](start: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext, sa: SchemaObject[A]): Operation[E, Set[Path[A]]] =
    instance.readOp {
      tree =>
        for {
          initial <- find(start, tree)
          erasedRes <- allShortestPathsImpl(initial, o => findPairsSetImpl(relationalQuery.tree.getUnsafe, Set(o), tree))
          res <- EitherOps.sequence(erasedRes.map(p => Path.from(p.toErasedPath, sa)))
        } yield res

    }

  override def insert[A, B](q: TraversableOnce[CompletedRelation[A, B]])(implicit e: ExecutionContext, sa: SchemaObject[A], sb: SchemaObject[B]): Operation[E, Unit] =
     instance.writeOp {
      t => q.foldLeft(t.right[E]){
        (eTree, r) => { // probably very slow
          eTree.flatMap(
            tree =>
              write(tree)(sa.tableName, sa.findable(r.a).getUnsafe, r.r.name, sb.tableName, sb.findable(r.b).getUnsafe)
          )
        }
      }
    }

}
