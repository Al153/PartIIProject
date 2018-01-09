package impl.memory

import core.user.interfaces.DBExecutor
import core.user.containers.{Operation, Path}
import core.user.dsl.{CompletedRelation, E}
import core.backend.intermediate._
import core.user.schema.{SchemaDescription, SchemaObject}
import core.utils._
import impl.memory.errors.{MemoryExtractError, MemoryMissingRelation}

import scalaz.Scalaz._


/**
  * Created by Al on 22/10/2017.
  *
  * Executor implementation
  */
class InMemoryExecutor(instance: MemoryInstance, schemaDescription: SchemaDescription) extends DBExecutor {

  /**
    * Implementation of findAll, uses methods implementation
    */
  override def findAll[A](q: FindSingle[A])(implicit sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Vector[A]] =
    instance.readOp(
      t =>
        for {
          unsafeQuery <- q.getUnsafe.leftMap(MemoryMissingRelation)
          v <- methods.findSingleImpl(unsafeQuery, t)
          res <- EitherOps.sequence(v.map(o => sa.fromRow(o.value).leftMap(MemoryExtractError)))
        } yield res
    )

  /**
    * Implementation of findAllPairs, uses methods implementation
    */
  override def findAllPairs[A, B](q: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Vector[(A, B)]] =
    instance.readOp {
      t =>
        for {
          // Setup
          unsafeSingle <- Find(q.sa.any)(q.sa, sd).getUnsafe.leftMap(MemoryMissingRelation)
          initial <- methods.findSingleImpl(unsafeSingle, t)
          unsafeQuery <-  q.getUnsafe.leftMap(MemoryMissingRelation)
          // find pairs of MemoryObjects
          v <- methods.findPairsImpl(unsafeQuery, initial, t)

          // Collect into pairs of (A, B)
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
    * Implementation of findDistinct, uses methods implementation
    */
  override def findDistinct[A](q: FindSingle[A])(implicit sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[A]] =
    instance.readOp(
      t =>
        for {
          // setup
          unsafeQuery <- q.getUnsafe.leftMap(MemoryMissingRelation)

          // find the MemoryObjects
          v <- methods.findSingleSetImpl(unsafeQuery, t)

          // Extract results
          res <- EitherOps.sequence(v.map(o => sa.fromRow(o.value).leftMap(MemoryExtractError)))
        } yield res
    )

  /**
    * Implementation of findDistinctPairs, uses methods implementation
    */

  override def findDistinctPairs[A, B](q: FindPair[A, B])(implicit  sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Set[(A, B)]] =
    instance.readOp {
      t =>
        for {
          // Setup
          unsafeQuery <- Find(q.sa.any)(q.sa, sd).getUnsafe.leftMap(MemoryMissingRelation)
          initial <- methods.findSingleSetImpl(unsafeQuery, t)
          unsafeQuery <- q.getUnsafe.leftMap(MemoryMissingRelation)

          // find pairs of MemoryObjects
          v <- methods.findPairsSetImpl(unsafeQuery, initial, t)

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
  override def shortestPath[A](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Option[Path[A]]] =
    instance.readOp {
      tree =>
        for {
          // setup
          initial <- methods.find(start, tree)
          unsafeQuery <- relationalQuery.tree.getUnsafe.leftMap(MemoryMissingRelation)

          // get a vector of results
          erasedRes <- methods.singleShortestsPathImpl(
            initial,
            sa.findable(end).getUnsafe,
            o => methods.findPairsSetImpl(unsafeQuery, Set(o), tree),
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

  override def allShortestPaths[A](start: A, relationalQuery: RelationalQuery[A, A])(implicit sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[Path[A]]] =
    instance.readOp {
      tree =>
        for {
          // setup
          initial <- methods.find(start, tree)
          unsafeQuery <- relationalQuery.tree.getUnsafe.leftMap(MemoryMissingRelation)

          // get erased path
          erasedRes <- methods.allShortestPathsImpl(initial, o => methods.findPairsSetImpl(unsafeQuery, Set(o), tree))

          // render to result
          res <- EitherOps.sequence(erasedRes.map(_.toPath[A]))
        } yield res

    }

  /**
    * Implementation of insert, uses methods implementation
    */
  override def insert[A, B](q: TraversableOnce[CompletedRelation[A, B]])(
    implicit sa: SchemaObject[A],
    sb: SchemaObject[B],
    sd: SchemaDescription
  ): Operation[E, Unit] =
    instance.writeOp {
      t =>
        q.foldLeft(MemoryEither(t)){ // inserts one by one
          (eTree, r) => { // probably very slow
            schemaDescription
              .getRelationName(r.r)
              .leftMap(MemoryMissingRelation)
              .flatMap {
                relationName =>
                  eTree.flatMap(
                    tree =>
                      methods.write(tree, sa.name, sa.getDBObject(r.a), relationName, sb.name, sb.getDBObject(r.b))
                  )
              }
          }
        }
    }

}
