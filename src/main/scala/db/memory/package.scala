package db

import core.RelationAttributes
import core.concrete.relations.CompletedRelation
import core.containers.{Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate.IntermediateTree
import db.interfaces.{DBExecutor, Extractor}
import schema.{RelationName, SchemaObject, TableName}
import view.View

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 20/10/2017.
  *
  * An in-memory database exectutor
  */
package object memory {
  implicit object InMemoryExecutor extends DBExecutor {
    override def findAll[A](t: IntermediateTree[A])(implicit e: ExecutionContext, ex: Extractor[A]): Operation[E, Vector[A]] = ???

    override def findDistinct[A](t: IntermediateTree[A])(implicit e: ExecutionContext): Operation[E, Set[A]] = ???

    override def shortestPath[A](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext, sa: SchemaObject[A]): Operation[E, Option[Path[A]]] = ???

    override def allShortestPaths[A](start: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext, sa: SchemaObject[A]): Operation[E, Set[Path[A]]] = ???

    override def insert[A, B](t: TraversableOnce[CompletedRelation[A, B, RelationAttributes[A, B]]])(implicit sa: SchemaObject[A], sb: SchemaObject[B]): Operation[E, Unit] = ???

    type MemoryTree = Map[TableName, MemoryTable]

    sealed trait MemoryTable {
      def objects: Vector[MemoryObject]
    }

    sealed trait MemoryObject {
      def relations: Map[RelationName, Vector[MemoryObject]]
    }


    var memoryStore: Map[View, MemoryTree] = ???
  }

}
