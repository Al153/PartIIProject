package core.user.dsl

import core.user.containers.{Operation, Path}
import core.user.interfaces.DBInstance
import core.user.schema.SchemaObject

/**
  * Created by Al on 04/10/2017.
  *
  *  A set of methods that can be used as commands, to simply DB commands
  */
trait Commands {
  /**
   * Find a set of distinct elements that match a query
   */

  def find[A](t: FindSingleAble[A])(implicit d: DBInstance, sa: SchemaObject[A]): Operation[E, Set[A]] = d.executor.find(t.toFindSingle)

  /**
   * Find distinct pairs that are related by the findpair
   */

  def findPairs[A, B](t: FindPairAble[A, B])(implicit d: DBInstance, sa: SchemaObject[A], sb: SchemaObject[B]): Operation[E, Set[(A, B)]] = d.executor.findPairs(t.toFindPair) // a multiset

  /**
   * given a pair of nodes and a relational query, try to find a path from start to end
   */

  def shortestPath[A](start: A, end: A, relationalQuery: FindPairAble[A, A])(implicit d: DBInstance, sa: SchemaObject[A]): Operation[E, Option[Path[A]]] = d.executor.shortestPath(start, end, relationalQuery.toFindPair) // find

  /**
   * find all shortest paths to nodes from a start node to reachable nodes
   */

  def allShortestPaths[A](start: A, relationalQuery: FindPairAble[A, A])(implicit d: DBInstance, sa: SchemaObject[A]): Operation[E, Set[Path[A]]] = d.executor.allShortestPaths(start, relationalQuery.toFindPair)

  /**
   * add a collection of relations to the database, creating a new [[core.user.dsl.View]]
   */

  def insert[A, B](t: TraversableOnce[CompletedRelation[A, B]])(implicit d: DBInstance, sa: SchemaObject[A], sb: SchemaObject[B]): Operation[E, Unit] = d.executor.insert(t) // Todo: Ensure relational query is full. maybe use m-(r)->n syntax for solid querie

  /**
    * add a collection of relations to the database, creating a new [[core.user.dsl.View]]
    */
  def insert[A, B](xs: CompletedRelation[A, B]*)(implicit d: DBInstance, sa: SchemaObject[A], sb: SchemaObject[B]): Operation[E, Unit] = insert(xs.toList)
}
