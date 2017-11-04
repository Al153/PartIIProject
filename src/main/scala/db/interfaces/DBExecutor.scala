package db.interfaces

import core.CompletedRelation
import core.containers.{Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate.{FindPair, FindSingle}
import schema.{SchemaDescription, SchemaObject}

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 14/10/2017.
  */
// Todo: the sets should probably be an arbitary Collection, to allow the backend to implement lazy collections

trait DBExecutor {
  /*
   * Find a multiset of all results that fit a particular query
   */
  def findAll[A](t: FindSingle[A])(implicit e: ExecutionContext, extractor: Extractor[A], sd: SchemaDescription): Operation[E, Vector[A]] // a multiset

  /*
   * Find a multiset of all pairs that match a relational query
   */

  def findAllPairs[A, B](t: FindPair[A, B])(implicit e: ExecutionContext, ea: Extractor[A], eb: Extractor[B], sd: SchemaDescription): Operation[E, Vector[(A, B)]] // a multiset

  /*
   * Find a set of distinct elements that match a query
   */

  def findDistinct[A](t: FindSingle[A])(implicit e: ExecutionContext, extractor: Extractor[A], sd: SchemaDescription): Operation[E, Set[A]]

  /*
   * Find a set of distinct pairs that match a query
   */

  def findDistinctPairs[A, B](t: FindPair[A, B])(implicit e: ExecutionContext, ea: Extractor[A], eb: Extractor[B], sd: SchemaDescription): Operation[E, Set[(A, B)]]

  /*
   * given a pair of nodes and a relational query, try to find a path from start to end
   */

  def shortestPath[A](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext, sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Option[Path[A]]]

  /*
   * find all shortest paths to nodes from a start node to reachable nodes
   */

  def allShortestPaths[A](start: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext, sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[Path[A]]]

  /*
   * add a collection of relations to the database, creating a new view
   */

  def insert[A, B](t: TraversableOnce[CompletedRelation[A, B]])(implicit e: ExecutionContext, sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Unit]
}
