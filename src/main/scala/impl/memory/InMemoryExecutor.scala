package impl.memory

import core.backend.intermediate._
import core.user.containers.{Operation, Path}
import core.user.dsl.{CompletedRelation, E}
import core.user.interfaces.DBExecutor
import core.user.schema.SchemaObject
import core.utils._
import impl.memory.errors.{MemoryExtractError, MemoryMissingRelation}
import impl.memory.methods.Methods


/**
  * Created by Al on 22/10/2017.
  *
  * Executor implementation
  */
class InMemoryExecutor(val instance: MemoryInstance) extends DBExecutor with Methods {
  /**
    * Implementation of findDistinct, uses methods implementation
    */
  override def find[A](q: FindSingle[A])(implicit sa: SchemaObject[A]): Operation[E, Set[A]] =
    instance.readOp(
      t =>
        for {
          // setup
          unsafeQuery <- q.getUnsafe(instance.schema).leftMap(MemoryMissingRelation)

          // find the MemoryObjects
          v <- findSingleSetImpl(unsafeQuery, t)

          // Extract results
          res <- EitherOps.sequence(v.map(o => sa.fromRow(o.value).leftMap(MemoryExtractError)))
        } yield res
    )

  /**
    * Implementation of findDistinctPairs, uses methods implementation
    */

  override def findPairs[A, B](q: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]): Operation[E, Set[(A, B)]] =
    instance.readOp {
      t =>
        for {
          // Setup
          unsafeQuery <- Find(q.sa.any).getUnsafe(instance.schema).leftMap(MemoryMissingRelation)
          initial <- findSingleSetImpl(unsafeQuery, t)
          unsafeQuery <- q.getUnsafe(instance.schema).leftMap(MemoryMissingRelation)

          // find pairs of MemoryObjects
          v <- findPairsSetImpl(unsafeQuery, initial, t)

          // extract results
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

  /**
    * Implementation of shortestPath, uses methods implementation
    */
  override def shortestPath[A](start: A, end: A, relationalQuery: FindPair[A, A])(implicit sa: SchemaObject[A]): Operation[E, Option[Path[A]]] =
    instance.readOp {
      tree =>
        for {
          // setup
          initial <- find(start, tree)
          unsafeQuery <- relationalQuery.getUnsafe(instance.schema).leftMap(MemoryMissingRelation)

          // get a vector of results
          erasedRes <- singleShortestsPathImpl(
            initial,
            sa.findable(end).getUnsafe,
            o => findPairsSetImpl(unsafeQuery, Set(o), tree).map(_.mapProj2),
            tree
          )
          // render to a path of As
          res <-  EitherOps.switch(erasedRes.map(
            _.toPath[A]
          ))
        } yield res
    }

  /**
    * Implementation of allShortestPaths, uses methods implementation
    */

  override def allShortestPaths[A](start: A, relationalQuery: FindPair[A, A])(implicit sa: SchemaObject[A]): Operation[E, Set[Path[A]]] =
    instance.readOp {
      tree =>
        for {
          // setup
          initial <- find(start, tree)
          unsafeQuery <- relationalQuery.getUnsafe(instance.schema).leftMap(MemoryMissingRelation)

          // get erased path
          erasedRes <- allShortestPathsImpl(initial, o => findPairsSetImpl(unsafeQuery, Set(o), tree).map(_.mapProj2))

          // render to result
          res <- EitherOps.sequence(erasedRes.map(_.toPath[A]))
        } yield res

    }

  /**
    * Implementation of insert, uses methods implementation
    */
  override def insert[A, B](q: TraversableOnce[CompletedRelation[A, B]])(
    implicit sa: SchemaObject[A],
    sb: SchemaObject[B]
  ): Operation[E, Unit] =
    instance.writeOp {
      t =>
        q.foldLeft(MemoryEither(t)){ // inserts one by one
          (eTree, r) => { // probably very slow
            instance.schema
              .getRelationName(r.r)
              .leftMap(MemoryMissingRelation)
              .flatMap {
                relationName =>
                  eTree.flatMap(
                    tree =>
                      write(tree, sa.name, sa.getDBObject(r.a), relationName, sb.name, sb.getDBObject(r.b))
                  )
              }
          }
        }
    }
}
