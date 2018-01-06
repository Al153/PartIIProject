package core.user.interfaces

import core.user.containers.{Operation, Path}
import core.user.dsl.{CompletedRelation, E}
import core.backend.intermediate.{FindPair, FindSingle, RelationalQuery}
import core.user.schema.{SchemaDescription, SchemaObject}

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 14/10/2017.
  */
// Todo: the sets should probably be an arbitary Collection, to allow the core.backend to implement lazy collections

trait DBExecutor {
  /*
   * Find a multiset of all results that fit a particular query
   */
  def findAll[A](t: FindSingle[A])(implicit extractor: SchemaObject[A], sd: SchemaDescription): Operation[E, Vector[A]] // a multiset

  /*
   * Find a multiset of all pairs that match a relational query
   */

  def findAllPairs[A, B](t: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Vector[(A, B)]] // a multiset

  /*
   * Find a set of distinct elements that match a query
   */

  def findDistinct[A](t: FindSingle[A])(implicit extractor: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[A]]

  /*
   * Find a set of distinct pairs that match a query
   */

  def findDistinctPairs[A, B](t: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Set[(A, B)]]

  /*
   * given a pair of nodes and a relational query, try to find a path from start to end
   */

  def shortestPath[A](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Option[Path[A]]]

  /*
   * find all shortest paths to nodes from a start node to reachable nodes
   */

  def allShortestPaths[A](start: A, relationalQuery: RelationalQuery[A, A])(implicit sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[Path[A]]]

  /*
   * add a collection of relations to the database, creating a new core.view
   */

  def insert[A, B](t: TraversableOnce[CompletedRelation[A, B]])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Unit]
}
