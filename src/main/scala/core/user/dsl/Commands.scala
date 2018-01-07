package core.user.dsl

import core.backend.intermediate.{Find, FindPair, FindSingle, RelationalQuery}
import core.user.containers.{Operation, Path}
import core.user.interfaces.DBInstance
import core.user.schema.{Findable, SchemaDescription, SchemaObject}

/**
  * Created by Al on 04/10/2017.
  */
trait Commands {
  /*
   * Find a multiset of all results that fit a particular query
   */
  def find[A](t: FindSingle[A])(implicit d: DBInstance, sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Vector[A]] = d.executor.findAll(t) // a multiset
  def find[A](q: UnaryQuery[A])(implicit d: DBInstance, sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Vector[A]] = d.executor.findAll(q.find)
  def find[A](f: Findable[A])(implicit d: DBInstance, sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Vector[A]] = find(Find(f))
  /*
   * Find all pair results
   */

  def findPairs[A, B](t: FindPair[A, B])(implicit d: DBInstance, sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Vector[(A, B)]] = d.executor.findAllPairs(t) // a multiset
  def findPairs[A, B](q: RelationalQuery[A, B])(implicit d: DBInstance, sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Vector[(A, B)]] = d.executor.findAllPairs(q.tree) // a multiset
  /*
   * Find a set of distinct elements that match a query
   */

  def findDistinct[A](t: FindSingle[A])(implicit d: DBInstance, sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[A]] = d.executor.findDistinct(t)
  def findDistinct[A](q: UnaryQuery[A])(implicit d: DBInstance, sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[A]] = d.executor.findDistinct(q.find)
  def findDistinct[A](f: Findable[A])(implicit d: DBInstance, sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[A]] = findDistinct(Find(f))
  /*
   * Find distinct pairs that are related by the findpair
   */
  def findPairsDistinct[A, B](t: FindPair[A, B])(implicit d: DBInstance, sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Set[(A, B)]] = d.executor.findDistinctPairs(t) // a multiset
  def findPairsDistinct[A, B](q: RelationalQuery[A, B])(implicit d: DBInstance, sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Set[(A, B)]] = d.executor.findDistinctPairs(q.tree) // a multiset

  /*
   * given a pair of nodes and a relational query, try to find a path from start to end
   */

  def shortestPath[A](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit d: DBInstance, sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Option[Path[A]]] = d.executor.shortestPath(start, end, relationalQuery) // find

  /*
   * find all shortest paths to nodes from a start node to reachable nodes
   */

  def allShortestPaths[A](start: A, relationalQuery: RelationalQuery[A, A])(implicit d: DBInstance, sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[Path[A]]] = d.executor.allShortestPaths(start, relationalQuery)

  /*
   * add a collection of relations to the database, creating a new core.view
   */

  def insert[A, B](t: TraversableOnce[CompletedRelation[A, B]])(implicit d: DBInstance, sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Unit] = d.executor.insert(t) // Todo: Ensure relational query is full. maybe use m-(r)->n syntax for solid querie
  def insert[A, B](xs: CompletedRelation[A, B]*)(implicit d: DBInstance, sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Unit] = insert(xs.toList)
}
