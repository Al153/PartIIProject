package core.backend.intermediate.unsafe

import core.user.dsl.Relation
import core.backend.intermediate.RelationName
import core.user.schema.TableName

/**
  * Created by Al on 25/10/2017.
  *
  * A type erased companion to [[Relation]]
  */
case class ErasedRelationAttributes(name: RelationName, from: TableName, to: TableName )

object ErasedRelationAttributes {
  def apply[A, B](name: RelationName, rab: Relation[A, B]): ErasedRelationAttributes = new ErasedRelationAttributes(name, rab.tableNames._1, rab.tableNames._2)
}