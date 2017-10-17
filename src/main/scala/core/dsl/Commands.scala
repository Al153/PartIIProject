package core.dsl

import core.{NodeDef, RelationAttributes}
import core.concrete.relations.CompletedRelation
import core.containers.{Operation, Path}
import core.error.E
import core.intermediate.IntermediateTree
import db.interfaces.DBExecutor


import scala.concurrent.ExecutionContext

/**
  * Created by Al on 04/10/2017.
  */
object Commands {
  /*
   * Find a multiset of all results that fit a particular query
   */
  def find[A](t: IntermediateTree[A])(implicit d: DBExecutor, e: ExecutionContext): Operation[E, Vector[A]] = d.findAll(t) // a multiset

  /*
   * Find a set of distinct elements that match a query
   */

  def findDistinct[A](t: IntermediateTree[A])(implicit d: DBExecutor, e: ExecutionContext): Operation[E, Set[A]] = d.findDistinct(t)

  /*
   * given a pair of nodes and a relational query, try to find a path from start to end
   */

  def shortestPath[A <: NodeDef](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit d: DBExecutor, e: ExecutionContext): Operation[E, Option[Path]] = d.shortestPath(start, end, relationalQuery) // find

  /*
   * find all shortest paths to nodes from a start node to reachable nodes
   */

  def allShortestPaths[A <: NodeDef](start: A, relationalQuery: RelationalQuery[A, A])(implicit d: DBExecutor, e: ExecutionContext): Operation[E, Set[Path]] = d.allShortestPaths(start, relationalQuery)

  /*
   * add a collection of relations to the database, creating a new view
   */

  def insert[A <: NodeDef, B <: NodeDef](t: TraversableOnce[CompletedRelation[A, B, RelationAttributes[A, B]]])(implicit d: DBExecutor, e: ExecutionContext): Operation[E, Unit] = d.insert(t) // Todo: Ensure relational query is full. maybe use m-(r)->n syntax for solid querie
}
