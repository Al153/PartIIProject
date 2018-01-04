package impl.memory

import core.backend.interfaces.DBExecutor
import core.containers.{Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate._
import core.relations.CompletedRelation
import core.schema.{SchemaDescription, SchemaObject}
import core.utils._
import impl.memory.errors.{MemoryExtractError, MemoryMissingRelation}

import scalaz.Scalaz._


/**
  * Created by Al on 22/10/2017.
  */
class InMemoryExecutor(instance: MemoryInstance, schemaDescription: SchemaDescription) extends DBExecutor {

  override def findAll[A](q: FindSingle[A])(implicit sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Vector[A]] =
    instance.readOp(
      t =>
        for {
          unsafeQuery <- q.getUnsafe.leftMap(MemoryMissingRelation)
          v <- methods.findSingleImpl(unsafeQuery, t)
          res <- EitherOps.sequence(v.map(o => sa.fromRow(o.value).leftMap(MemoryExtractError)))
        } yield res
    )

  override def findAllPairs[A, B](q: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Vector[(A, B)]] =
    instance.readOp {
      t =>
        for {
          unsafeSingle <- Find(q.sa.generalPattern)(q.sa, sd).getUnsafe.leftMap(MemoryMissingRelation)
          initial <- methods.findSingleImpl(unsafeSingle, t)
          unsafeQuery <-  q.getUnsafe.leftMap(MemoryMissingRelation)
          v <- methods.findPairsImpl(unsafeQuery, initial, t)
          res <- EitherOps.sequence(
            v.map {
              case (l, r) =>
                for {
                  a <- sa.fromRow(l.value).leftMap(MemoryExtractError)
                  b <- sb.fromRow(r.value).leftMap(MemoryExtractError)
                } yield (a, b)
            })
        } yield res
    }

  override def findDistinct[A](q: FindSingle[A])(implicit sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[A]] =
    instance.readOp(
      t =>
        for {
          unsafeQuery <- q.getUnsafe.leftMap(MemoryMissingRelation)
          v <- methods.findSingleSetImpl(unsafeQuery, t)
          res <- EitherOps.sequence(v.map(o => sa.fromRow(o.value).leftMap(MemoryExtractError)))
        } yield res
    )

  override def findDistinctPairs[A, B](q: FindPair[A, B])(implicit  sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Set[(A, B)]] =
    instance.readOp {
      t =>
        for {
          unsafeQuery <- Find(q.sa.generalPattern)(q.sa, sd).getUnsafe.leftMap(MemoryMissingRelation)
          initial <- methods.findSingleSetImpl(unsafeQuery, t)
          unsafeQuery <- q.getUnsafe.leftMap(MemoryMissingRelation)
          v <- methods.findPairsSetImpl(unsafeQuery, initial, t)
          res <- EitherOps.sequence(
            v.map {
              case (l, r) =>
                for {
                  a <- sa.fromRow(l.value).leftMap(MemoryExtractError)
                  b <- sb.fromRow(r.value).leftMap(MemoryExtractError)
                } yield (a, b)
            })
        } yield res
    }

  override def shortestPath[A](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Option[Path[A]]] =
    instance.readOp {
      tree =>
        for {
          initial <- methods.find(start, tree)
          unsafeQuery <- relationalQuery.tree.getUnsafe.leftMap(MemoryMissingRelation)
          erasedRes <- methods.singleShortestsPathImpl(
            initial,
            sa.findable(end).getUnsafe,
            o => methods.findPairsSetImpl(unsafeQuery, Set(o), tree),
            tree
          )
          res <-  EitherOps.switch(erasedRes.map(
            r => Path.from(r.toErasedPath, sa).leftMap(MemoryExtractError)
          ))
        } yield res
    }

  override def allShortestPaths[A](start: A, relationalQuery: RelationalQuery[A, A])(implicit sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[Path[A]]] =
    instance.readOp {
      tree =>
        for {
          initial <- methods.find(start, tree)
          unsafeQuery <- relationalQuery.tree.getUnsafe.leftMap(MemoryMissingRelation)
          erasedRes <- methods.allShortestPathsImpl(initial, o => methods.findPairsSetImpl(unsafeQuery, Set(o), tree))
          res <- EitherOps.sequence(erasedRes.map(
            p => Path.from(p.toErasedPath, sa).leftMap(MemoryExtractError))
          )
        } yield res

    }

  override def insert[A, B](q: TraversableOnce[CompletedRelation[A, B]])(
    implicit sa: SchemaObject[A],
    sb: SchemaObject[B],
    sd: SchemaDescription
  ): Operation[E, Unit] =
    instance.writeOp {
      t =>
        q.foldLeft(MemoryEither(t)){
          (eTree, r) => { // probably very slow
            schemaDescription
              .getRelationName(r.r)
              .leftMap(MemoryMissingRelation)
              .flatMap {
                relationName =>
                  eTree.flatMap(
                    tree =>
                      methods.write(tree, sa.tableName, sa.getDBObject(r.a), relationName, sb.tableName, sb.getDBObject(r.b))
                  )
              }
          }
        }
    }

}
