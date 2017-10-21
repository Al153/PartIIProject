package core

import core.dsl.RelationalQuery
import schema.{Findable, SchemaObject}

/**
  * Created by Al on 09/10/2017.
  */
package object intermediate {

  // IntermediateTree[A] is a query that returns an A
  sealed trait IntermediateTree[A] {}

  // Initial node find
  case class GetNode[A](p: Findable[A])(implicit sa: SchemaObject[A]) extends IntermediateTree[A]

  // Find a relation
  case class GetRelation[A, B, R](a: IntermediateTree[A], r: R)(implicit f: R => RelationAttributes[A, B], sa: SchemaObject[A], sb: SchemaObject[B]) extends IntermediateTree[(A, B)]

  // Chain a relation onto an existing one
  case class Chain[A, B, C](
                       left: IntermediateTree[(A, B)],
                       r: RelationalQuery[B, C]
                     )(implicit sa: SchemaObject[A], sb: SchemaObject[B], sc: SchemaObject[C]) extends IntermediateTree[(A, C)]

  // Not to be used
  case class Join[A, B, C](left: IntermediateTree[(A, B)], r: IntermediateTree[(B, C)])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sc: SchemaObject[C]) extends IntermediateTree[(A, C)]

  // Do nothing and return an empty set of results
  case class Pass[A]() extends IntermediateTree[A]

  // Intersection of the results given by the trees
  case class Intersection[A](left: IntermediateTree[A], right: IntermediateTree[A]) extends IntermediateTree[A]

  // Union of results given by the trees
  case class Union[A](left: IntermediateTree[A], right: IntermediateTree[A]) extends IntermediateTree[A]

  // filter results by a pattern. Semantically should be the same as a Union, but tagged so as to imply less work
  case class Narrow[A, B](t: IntermediateTree[(A, B)], p: Findable[B]) extends IntermediateTree[(A, B)]

  // transitive relation causes denormalisation against a view to get a cached value
  case class Transitive[A](intermediateTree: IntermediateTree[A], f: A => Any) extends IntermediateTree[Any] // todo fill in

  // Classes for projecting types
  case class Proj1[A, B](ab: IntermediateTree[(A, B)]) extends IntermediateTree[A]
  case class Proj2[A, B](ab: IntermediateTree[(A, B)]) extends IntermediateTree[B]

  // combine two trees to form a pair
  case class Pair[A, B](a: IntermediateTree[A], b: IntermediateTree[B]) extends IntermediateTree[(A, B)]

  // duplicating types - only need to search once to get both. mostly for type system manipulation/optimisation.
  case class Dup[A](a: IntermediateTree[A]) extends IntermediateTree[(A, A)]
  // TODO: Implementors

}
