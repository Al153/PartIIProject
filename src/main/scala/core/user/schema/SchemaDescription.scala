package core.user.schema

import core.backend.intermediate.unsafe.{ErasedRelationAttributes, SchemaObjectErased}
import core.backend.common.{ExtractError, MissingRelation, MissingTableName}
import core.user.dsl.{E, RelationAttributes}
import core.backend.intermediate.RelationName
import core.utils._

import scalaz._
import Scalaz._
/**
  * Created by Al on 21/10/2017.
  *
  * Used by a core.backend to generate core.user.schema
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

  val objectMap: Map[TableName, SchemaObjectErased]  = erasedObjects.map(o => o.name -> o).toMap

  def lookupTable(t: TableName): ExtractError \/ SchemaObjectErased = objectMap.getOrError(t, MissingTableName(t))

  def erasedRelations: Set[ErasedRelationAttributes] = relationMap.values.toSet

  def getRelation[A, B](r: RelationAttributes[A, B]): MissingRelation \/ ErasedRelationAttributes =
    relationMap.getOrError(r, MissingRelation(r))
  def getRelationName[A, B](r: RelationAttributes[A, B]): MissingRelation \/ RelationName = getRelation(r).map(_.name)
  def tableNames: Set[TableName] = objects.map(_.tableName)
}



