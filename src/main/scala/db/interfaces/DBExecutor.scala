package db.interfaces

import core.{NodeDef, RelationAttributes}
import core.concrete.relations.CompletedRelation
import core.containers.{Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate.IntermediateTree
import view.View

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 14/10/2017.
  */
trait DBExecutor {
  /*
   * Find a multiset of all results that fit a particular query
   */
  def findAll[A](t: IntermediateTree[A])(implicit e: ExecutionContext): Operation[E, Vector[A]] // a multiset

  /*
   * Find a set of distinct elements that match a query
   */

  def findDistinct[A](t: IntermediateTree[A])(implicit e: ExecutionContext): Operation[E, Set[A]]

  /*
   * given a pair of nodes and a relational query, try to find a path from start to end
   */

  def shortestPath[A <: NodeDef](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext): Operation[E, Option[Path]]

  /*
   * find all shortest paths to nodes from a start node to reachable nodes
   */

  def allShortestPaths[A <: NodeDef](start: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext): Operation[E, Set[Path]]

  /*
   * add a collection of relations to the database, creating a new view
   */

  def insert[A, B](t: TraversableOnce[CompletedRelation[A, B, RelationAttributes[A, B]]]): Operation[E, Unit]
  // Todo: Ensure relational query is full. maybe use m-(r)->n syntax for solid querie
}
