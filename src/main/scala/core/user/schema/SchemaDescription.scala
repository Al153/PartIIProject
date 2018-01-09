package core.user.schema

import core.backend.intermediate.unsafe.ErasedRelationAttributes
import core.backend.common.{ExtractError, MissingRelation, MissingTableName}
import core.user.dsl.{E, RelationAttributes}
import core.backend.intermediate.RelationName
import core.utils._

import scalaz._
import Scalaz._
/**
  * Created by Al on 21/10/2017.
  *
  * An object which holds all the lookup information for the schema of the [[core.user.interfaces.DBBackend]]
  */
final class SchemaDescription(
                               val objects: Set[SchemaObject[_]],
                               val relations: Set[RelationAttributes[_, _]]
                       ) {

  val relationMap: Map[RelationAttributes[_, _], ErasedRelationAttributes] = relations.zipWithIndex.map { case (r, index) =>
    val from = r.sa.name
    val to = r.sb.name
    r -> ErasedRelationAttributes(RelationName(index.toString), from, to)
  }.toMap

  val objectMap: Map[TableName, SchemaObject[_]]  = objects.map(o => o.name -> o).toMap

  def lookupTable(t: TableName): ExtractError \/ SchemaObject[_] = objectMap.getOrError(t, MissingTableName(t))

  def erasedRelations: Set[ErasedRelationAttributes] = relationMap.values.toSet

  def getRelation[A, B](r: RelationAttributes[A, B]): MissingRelation \/ ErasedRelationAttributes =
    relationMap.getOrError(r, MissingRelation(r))
  def getRelationName[A, B](r: RelationAttributes[A, B]): MissingRelation \/ RelationName = getRelation(r).map(_.name)
  def tableNames: Set[TableName] = objects.map(_.name)
}




