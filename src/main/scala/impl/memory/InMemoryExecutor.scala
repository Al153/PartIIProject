package impl.memory

import core.backend.common.EmptyFringeError
import core.backend.interfaces.DBExecutor
import core.containers.{Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate._
import core.relations.CompletedRelation
import core.schema.{SchemaDescription, SchemaObject}
import core.utils._

import scalaz.Scalaz._


/**
  * Created by Al on 22/10/2017.
  */
class InMemoryExecutor(instance: MemoryInstance, schemaDescription: SchemaDescription) extends DBExecutor {

  override def findAll[A](q: FindSingle[A])(implicit sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Vector[A]] =
    instance.readOp(
      t =>
        for {
          unsafeQuery <- q.getUnsafe
          v <- methods.findSingleImpl(unsafeQuery, t)
          res <- EitherOps.sequence(v.map(o => sa.fromRow(o.value)))
        } yield res
    )

  override def findAllPairs[A, B](q: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Vector[(A, B)]] =
    instance.readOp {
      t =>
        for {
          unsafeSingle <- Find(q.sa.generalPattern)(q.sa, sd).getUnsafe
          initial <- methods.findSingleImpl(unsafeSingle, t)
          unsafeQuery <-  q.getUnsafe
          v <- methods.findPairsImpl(unsafeQuery, initial, t)
          res <- EitherOps.sequence(
            v.map {
              case (l, r) =>
                for {
                  a <- sa.fromRow(l.value)
                  b <- sb.fromRow(r.value)
                } yield (a, b)
            })
        } yield res
    }

  override def findDistinct[A](q: FindSingle[A])(implicit sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[A]] =
    instance.readOp(
      t =>
        for {
          unsafeQuery <- q.getUnsafe
          v <- methods.findSingleSetImpl(unsafeQuery, t)
          res <- EitherOps.sequence(v.map(o => sa.fromRow(o.value)))
        } yield res
    )

  override def findDistinctPairs[A, B](q: FindPair[A, B])(implicit  sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Set[(A, B)]] =
    instance.readOp {
      t =>
        for {
          unsafeQuery <- Find(q.sa.generalPattern)(q.sa, sd).getUnsafe
          initial <- methods.findSingleSetImpl(unsafeQuery, t)
          unsafeQuery <- q.getUnsafe
          v <- methods.findPairsSetImpl(unsafeQuery, initial, t)
          res <- EitherOps.sequence(
            v.map {
              case (l, r) =>
                for {
                  a <- sa.fromRow(l.value)
                  b <- sb.fromRow(r.value)
                } yield (a, b)
            })
        } yield res
    }

  override def shortestPath[A](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Option[Path[A]]] =
    instance.readOp {
      tree =>
        for {
          initial <- methods.find(start, tree)
          unsafeQuery <- relationalQuery.tree.getUnsafe
          erasedRes <- methods.singleShortestsPathImpl(initial, sa.findable(end).getUnsafe, o => methods.findPairsSetImpl(unsafeQuery, Set(o), tree), tree)
          res <-  erasedRes.map(r => Path.from(r.toErasedPath, sa)).fold(Option.empty[Path[A]].right[E])(_.map(_.some))
        } yield res
    }

  override def allShortestPaths[A](start: A, relationalQuery: RelationalQuery[A, A])(implicit sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[Path[A]]] =
    instance.readOp {
      tree =>
        for {
          initial <- methods.find(start, tree)
          unsafeQuery <- relationalQuery.tree.getUnsafe
          erasedRes <- methods.allShortestPathsImpl(initial, o => methods.findPairsSetImpl(unsafeQuery, Set(o), tree))
          res <- EitherOps.sequence(erasedRes.map(p => Path.from(p.toErasedPath, sa)))
        } yield res

    }

  override def insert[A, B](q: TraversableOnce[CompletedRelation[A, B]])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Unit] =
    instance.writeOp {
      t =>
        q.foldLeft(t.right[E]){
          (eTree, r) => { // probably very slow
            schemaDescription.getRelationName(r.r).flatMap {
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
