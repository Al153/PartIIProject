package impl.memory

import core.backend.common.DBObject
import core.backend.intermediate.RelationName
import core.user.schema.TableName

/**
  * Created by Al on 25/10/2017.
  */

case class MemoryObject(
                         value: DBObject,
                         tableName: TableName,
                         relations: Map[RelationName, Set[DBObject]],
                         revRelations: Map[RelationName, Set[DBObject]]
                       ) {

  private[MemoryObject] val hashable = MemoryObject.Hashable(value, tableName) // this is a class holding the bits we want to compare

  def addRelation(relation: RelationName, that: DBObject): MemoryObject =
    if (relations.contains(relation)) {
      MemoryObject(value, tableName, relations + (relation -> (relations(relation) + that)), revRelations)
    } else {
      MemoryObject(value, tableName, relations + (relation -> Set(that)), revRelations)
    }

  def addReverseRelation(relation: RelationName, that: DBObject): MemoryObject =
    if (revRelations.contains(relation)) {
      MemoryObject(value, tableName, relations, revRelations+ (relation -> (revRelations(relation) + that)))
    } else {
      MemoryObject(value, tableName, relations, revRelations + (relation -> Set(that)))
    }

  def getRelated(relationName: RelationName): Set[DBObject] = relations.getOrElse(relationName, Set())
  def getRevRelated(relationName: RelationName): Set[DBObject] = revRelations.getOrElse(relationName, Set())

  override def toString: String = "[" +  value.toString + "]"

  /**
    * Both equals and hashcode look only at the DBObject part of the object.
    * This is to make updating of tables easier
    * @return
    */

  override def hashCode(): Int = hashable.hashCode()


  override def equals(obj: scala.Any): Boolean = obj match {
    case o: MemoryObject => o.hashable == this.hashable
    case _ => false
  }
}

object MemoryObject {
  private[MemoryObject] case class Hashable(value: DBObject, name: TableName)
}