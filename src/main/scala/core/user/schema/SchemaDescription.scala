package core.user.schema

import core.backend.common.{ExtractError, MissingRelation, MissingTableName}
import core.backend.intermediate.RelationName
import core.backend.intermediate.unsafe.ErasedRelationAttributes
import core.user.dsl.RelationAttributes
import core.utils._

import scalaz._
/**
  * Created by Al on 21/10/2017.
  *
  * An object which holds all the lookup information for the schema of the [[core.user.interfaces.DBBackend]]
  */
final class SchemaDescription(
                               val objects: Set[SchemaObject[_]],
                               val relations: Set[RelationAttributes[_, _]]
                       ) {

  /**
    * Gives each relation a guaranteed unique name
    */
  val relationMap: Map[RelationAttributes[_, _], ErasedRelationAttributes] = relations.zipWithIndex.map { case (r, index) =>
    val from = r.sa.name
    val to = r.sb.name
    r -> ErasedRelationAttributes(RelationName(index.toString), from, to)
  }.toMap


  /**
    * a lookup table for [[SchemaObject]]s
    */
  val objectMap: Map[TableName, SchemaObject[_]]  = objects.map(o => o.name -> o).toMap

  /**
    * Safely lookup a tableName for its [[SchemaObject]]
    */
  def lookupTable(t: TableName): ExtractError \/ SchemaObject[_] = objectMap.getOrError(t, MissingTableName(t))

  /**
    * ErasedRelationAttributes that are contained in this schema
    * @return
    */
  def erasedRelations: Set[ErasedRelationAttributes] = relationMap.values.toSet

  /**
    * Safely lookup a relation attributes to get its erased (named) equivalent
    */
  def getRelation[A, B](r: RelationAttributes[A, B]): MissingRelation \/ ErasedRelationAttributes =
    relationMap.getOrError(r, MissingRelation(r))

  /**
    * Get the name of a relation from its relational attributes
    */
  def getRelationName[A, B](r: RelationAttributes[A, B]): MissingRelation \/ RelationName = getRelation(r).map(_.name)
}




