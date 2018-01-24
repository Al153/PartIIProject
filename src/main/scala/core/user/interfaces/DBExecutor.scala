package core.user.interfaces

import core.backend.intermediate.{FindPair, FindSingle}
import core.user.containers.{Operation, Path}
import core.user.dsl.{CompletedRelation, E}
import core.user.schema.SchemaObject

/**
  * Created by Al on 14/10/2017.
  *
  * Core operations a [[DBInstance]] should implement
  */
// Todo: the sets should probably be an arbitrary Collection, to allow the backend to implement lazy collections

trait DBExecutor {
  /**
   * Find a set of distinct elements that match a query
   */

  def find[A](t: FindSingle[A])(implicit extractor: SchemaObject[A]): Operation[E, Set[A]]

  /**
   * Find a set of distinct pairs that match a query
   */

  def findPairs[A, B](t: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]): Operation[E, Set[(A, B)]]

  /**
   * given a pair of nodes and a relational query, try to find a path from start to end
   */

  def shortestPath[A](start: A, end: A, relationalQuery: FindPair[A, A])(implicit sa: SchemaObject[A]): Operation[E, Option[Path[A]]]

  /**
   * find all shortest paths to nodes from a start node to reachable nodes
   */

  def allShortestPaths[A](start: A, relationalQuery: FindPair[A, A])(implicit sa: SchemaObject[A]): Operation[E, Set[Path[A]]]

  /**
   * add a collection of relations to the database, creating a new core.view
   */

  def insert[A, B](t: TraversableOnce[CompletedRelation[A, B]])(implicit sa: SchemaObject[A], sb: SchemaObject[B]): Operation[E, Unit]
}
