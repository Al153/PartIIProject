package db.memory

import core.CompletedRelation
import core.containers.{Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate._
import db.interfaces.{DBExecutor, Extractor}
import schema.{SchemaDescription, SchemaObject}
import utils._

import scala.concurrent.ExecutionContext
import scalaz.Scalaz._
import scalaz._


/**
  * Created by Al on 22/10/2017.
  */
class InMemoryExecutor(instance: MemoryInstance, schemaDescription: SchemaDescription) extends DBExecutor {
  override def findAll[A](q: FindSingle[A])(implicit e: ExecutionContext, ea: Extractor[A], sd: SchemaDescription): Operation[E, Vector[A]] =
    instance.readOp(
      t =>
        for {
          unsafeQuery <- q.getUnsafe
          v <- findSingleImpl(unsafeQuery, t)
          res <- EitherOps.sequence(v.map(o => ea.fromRow(o.value)))
        } yield res
    )

  override def findAllPairs[A, B](q: FindPair[A, B])(implicit e: ExecutionContext, ea: Extractor[A], eb: Extractor[B], sd: SchemaDescription): Operation[E, Vector[(A, B)]] =
    instance.readOp {
      t =>
        for {
          unsafeSingle <- Find(q.sa.generalPattern)(q.sa, sd).getUnsafe
          initial <- {println("Single = " + unsafeSingle); findSingleImpl(unsafeSingle, t)}
          unsafeQuery <- {println("initial = " + initial) ; q.getUnsafe}
          v <- {println("query = " + unsafeQuery) ; findPairsImpl(unsafeQuery, initial, t)}
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

  override def findDistinct[A](q: FindSingle[A])(implicit e: ExecutionContext, ea: Extractor[A], sd: SchemaDescription): Operation[E, Set[A]] =
    instance.readOp(
      t =>
        for {
          unsafeQuery <- q.getUnsafe
          v <- findSingleSetImpl(unsafeQuery, t)
          res <- EitherOps.sequence(v.map(o => ea.fromRow(o.value)))
        } yield res
    )

  override def findDistinctPairs[A, B](q: FindPair[A, B])(implicit e: ExecutionContext, ea: Extractor[A], eb: Extractor[B], sd: SchemaDescription): Operation[E, Set[(A, B)]] =
    instance.readOp {
      t =>
        for {
          unsafeQuery <- Find(q.sa.generalPattern)(q.sa, sd).getUnsafe
          initial <- findSingleSetImpl(unsafeQuery, t)
          unsafeQuery <- q.getUnsafe
          v <- findPairsSetImpl(unsafeQuery, initial, t)
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

  override def shortestPath[A](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext, sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Option[Path[A]]] =
    instance.readOp {
      tree =>
        for {
          initial <- find(start, tree)
          unsafeQuery <- relationalQuery.tree.getUnsafe
          erasedRes <- singleShortestsPathImpl(initial, sa.findable(end).getUnsafe, o => findPairsSetImpl(unsafeQuery, Set(o), tree))
          res <-  erasedRes.map(r => Path.from(r.toErasedPath, sa)).fold(Option.empty[Path[A]].right[E])(_.map(_.some))
        } yield res
    }

  override def allShortestPaths[A](start: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext, sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[Path[A]]] =
    instance.readOp {
      tree =>
        for {
          initial <- find(start, tree)
          unsafeQuery <- relationalQuery.tree.getUnsafe
          erasedRes <- allShortestPathsImpl(initial, o => findPairsSetImpl(unsafeQuery, Set(o), tree))
          res <- EitherOps.sequence(erasedRes.map(p => Path.from(p.toErasedPath, sa)))
        } yield res

    }

  override def insert[A, B](q: TraversableOnce[CompletedRelation[A, B]])(implicit e: ExecutionContext, sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Unit] =
    instance.writeOp {
      t =>
        q.foldLeft(t.right[E]){
          (eTree, r) => { // probably very slow
            schemaDescription.getRelationName(r.r).flatMap {
              relationName =>
                eTree.flatMap(
                  tree =>
                    write(tree, sa.tableName, sa.findable(r.a).getUnsafe, relationName, sb.tableName, sb.findable(r.b).getUnsafe)
                )
            }
          }
        }
    }

}
