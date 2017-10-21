package db.interfaces

import core.RelationAttributes
import core.concrete.relations.CompletedRelation
import core.containers.{Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate.IntermediateTree
import schema.SchemaObject

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 14/10/2017.
  */
trait DBExecutor {
  /*
   * Find a multiset of all results that fit a particular query
   */
  def findAll[A](t: IntermediateTree[A])(implicit e: ExecutionContext, extractor: Extractor[A]): Operation[E, Vector[A]] // a multiset

  /*
   * Find a set of distinct elements that match a query
   */

  def findDistinct[A](t: IntermediateTree[A])(implicit e: ExecutionContext): Operation[E, Set[A]]

  /*
   * given a pair of nodes and a relational query, try to find a path from start to end
   */

  def shortestPath[A](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext, sa: SchemaObject[A]): Operation[E, Option[Path[A]]]

  /*
   * find all shortest paths to nodes from a start node to reachable nodes
   */

  def allShortestPaths[A](start: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext, sa: SchemaObject[A]): Operation[E, Set[Path[A]]]

  /*
   * add a collection of relations to the database, creating a new view
   */

  def insert[A, B](t: TraversableOnce[CompletedRelation[A, B, RelationAttributes[A, B]]])(implicit sa: SchemaObject[A], sb: SchemaObject[B]): Operation[E, Unit]
  // Todo: Ensure relational query is full. maybe use m-(r)->n syntax for solid querie
}
