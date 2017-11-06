package core.schema

import core.error.E
import core.intermediate.unsafe.{ErasedRelationAttributes, SchemaObjectErased}
import core.relations.RelationAttributes
import core.backend.common.MissingRelation
import core.utils._

import scalaz._
import Scalaz._
/**
  * Created by Al on 21/10/2017.
  *
  * Used by a core.backend to generate core.schema
  */
final class SchemaDescription(
                               objects: Set[SchemaObject[_]],
                               val relations: Set[RelationAttributes[_, _]]
                       ) {
  // contains a set of SchemaObjects and a set of RelationalAttributes

  val erasedObjects: Set[SchemaObjectErased] = objects.map(_.erased)
  val relationMap: Map[RelationAttributes[_, _], ErasedRelationAttributes] = relations.zipWithIndex.map { case (r, index) =>
    val from = r.sa.tableName
    val to = r.sb.tableName
    r -> ErasedRelationAttributes(RelationName(index.toString), from, to)
  }.toMap

  def erasedRelations: Set[ErasedRelationAttributes] = relationMap.values.toSet
  def getRelation[A, B](r: RelationAttributes[A, B]): E \/ ErasedRelationAttributes =
    relationMap.getOrError(r, MissingRelation(r))
  def getRelationName[A, B](r: RelationAttributes[A, B]): E \/ RelationName = getRelation(r).map(_.name)
}




