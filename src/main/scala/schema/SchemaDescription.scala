package schema

import core.RelationAttributes
import core.error.E
import core.intermediate.unsafe.{ErasedRelationAttributes, SchemaObjectErased}
import db.common.MissingRelation

import scalaz._, Scalaz._
/**
  * Created by Al on 21/10/2017.
  *
  * Used by a backend to generate schema
  */
final class SchemaDescription(
                         val objects: Set[SchemaObjectErased],
                         val relations: Set[RelationAttributes[_, _]]
                       ) {
  // contains a set of SchemaObjects and a set of RelationalAttributes

  val relationMap: Map[RelationAttributes[_, _], ErasedRelationAttributes] = relations.zipWithIndex.map { case (r, index) =>
    val from = r.sa.tableName
    val to = r.sb.tableName
    r -> ErasedRelationAttributes(RelationName(index.toString), from, to)
  }.toMap

  def erasedRelations: Set[ErasedRelationAttributes] = relationMap.values.toSet
  def getRelation[A, B](r: RelationAttributes[A, B]): E \/ ErasedRelationAttributes =
    relationMap.get(r).fold(\/.left[E, ErasedRelationAttributes](MissingRelation(r)))(_.right[E])
  def getRelationName[A, B](r: RelationAttributes[A, B]): E \/ RelationName = getRelation(r).map(_.name)
}




