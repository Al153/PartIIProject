package db

import core.{NodeDef, RelationAttributes}
import core.concrete.relations.CompletedRelation
import core.containers.{Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate.IntermediateTree
import db.interfaces.{DBExecutor, Extractor}
import schema.{SchemaObject, TableName}
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

    override def shortestPath[A <: NodeDef](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext): Operation[E, Option[Path]] = ???

    override def allShortestPaths[A <: NodeDef](start: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext): Operation[E, Set[Path]] = ???

    override def insert[A <: NodeDef, B <: NodeDef](t: TraversableOnce[CompletedRelation[A, B, RelationAttributes[A, B]]]): Operation[E, Unit] = ???

    type MemoryTree = Map[TableName, MemoryTable]

    sealed trait MemoryTable {
      def objects: Vector[MemoryObject]
    }

    implicit def extract[A](implicit so: SchemaObject[A]): Extractor[A] = new Extractor[A] {
      val summaray = so.getSchemaSummary
      override def fromRow = ???
    }



    sealed trait MemoryObject {
      def relations: Map[RelationName, Vector[MemoryObject]]
    }


    var memoryStore: Map[View, MemoryTree]
  }

}
