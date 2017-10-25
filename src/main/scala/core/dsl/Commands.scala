package core.dsl

import core.concrete.relations.CompletedRelation
import core.containers.{Operation, Path}
import core.error.E
import core.intermediate.{FindPair, FindSingle}
import db.interfaces.{DBInstance, Extractor}
import schema.SchemaObject

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 04/10/2017.
  */
object Commands {
  /*
   * Find a multiset of all results that fit a particular query
   */
  def find[A](t: FindSingle[A])(implicit d: DBInstance, e: ExecutionContext, ea: Extractor[A]): Operation[E, Vector[A]] = d.executor.findAll(t) // a multiset

  /*
   * Find all pair results
   */

  def findPairs[A, B](t: FindPair[A, B])(implicit d: DBInstance, e: ExecutionContext, ea: Extractor[A], eb: Extractor[B]): Operation[E, Vector[(A, B)]] = d.executor.findAllPairs(t) // a multiset

  /*
   * Find a set of distinct elements that match a query
   */

  def findDistinct[A](t: FindSingle[A])(implicit d: DBInstance, e: ExecutionContext): Operation[E, Set[A]] = d.executor.findDistinct(t)
  /*
   * Find distinct pairs that are related by the findpair
   */
  def findPair[A, B](t: FindPair[A, B])(implicit d: DBInstance, e: ExecutionContext, ea: Extractor[A], eb: Extractor[B]): Operation[E, Vector[(A, B)]] = d.executor.findAllPairs(t) // a multiset


  /*
   * given a pair of nodes and a relational query, try to find a path from start to end
   */

  def shortestPath[A](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit d: DBInstance, e: ExecutionContext, sa: SchemaObject[A]): Operation[E, Option[Path[A]]] = d.executor.shortestPath(start, end, relationalQuery) // find

  /*
   * find all shortest paths to nodes from a start node to reachable nodes
   */

  def allShortestPaths[A](start: A, relationalQuery: RelationalQuery[A, A])(implicit d: DBInstance, e: ExecutionContext, sa: SchemaObject[A]): Operation[E, Set[Path[A]]] = d.executor.allShortestPaths(start, relationalQuery)

  /*
   * add a collection of relations to the database, creating a new view
   */

  def insert[A, B](t: TraversableOnce[CompletedRelation[A, B]])(implicit d: DBInstance, e: ExecutionContext, sa: SchemaObject[A], sb: SchemaObject[B]): Operation[E, Unit] = d.executor.insert(t) // Todo: Ensure relational query is full. maybe use m-(r)->n syntax for solid querie
}
