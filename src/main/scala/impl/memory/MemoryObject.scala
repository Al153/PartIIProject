package impl.memory

import core.backend.common.DBObject
import core.backend.intermediate.RelationName
import core.backend.intermediate.unsafe.ErasedRelationAttributes
import core.user.schema.TableName
import core.utils.EitherOps

/**
  * Created by Al on 25/10/2017.
  *
  * An object to be stored in a [[MemoryTable]]
  *
  * Carries out the roles of storing fields and recording relations
  */

case class MemoryObject(
                         value: DBObject,
                         tableName: TableName,
                         relations: Map[RelationName, Set[DBObject]],
                         revRelations: Map[RelationName, Set[DBObject]]
                       ) {

  /**
    * When comparing [[MemoryObject]]s, we're only interested in the underlying [[DBObject]] and the [[TableName]]
    */

  private[MemoryObject] val hashable = MemoryObject.Hashable(value, tableName) // this is a class holding the bits we want to compare

  /**
    * Gets the Memory object but with an additional relation added
    */
  def addRelation(relation: RelationName, that: DBObject): MemoryObject =
    if (relations.contains(relation)) {
      MemoryObject(value, tableName, relations + (relation -> (relations(relation) + that)), revRelations)
    } else {
      MemoryObject(value, tableName, relations + (relation -> Set(that)), revRelations)
    }

  /**
    * Gets the Memory object but with an additional reverse relation added
    */
  def addReverseRelation(relation: RelationName, that: DBObject): MemoryObject =
    if (revRelations.contains(relation)) {
      MemoryObject(value, tableName, relations, revRelations+ (relation -> (revRelations(relation) + that)))
    } else {
      MemoryObject(value, tableName, relations, revRelations + (relation -> Set(that)))
    }

  /**
    * Get all objects related by a given relation from this object
    */
  def getRelated(relationName: RelationName): Set[DBObject] = relations.getOrElse(relationName, Set())

  /**
    * get all MemoryObjects related by a given relation from this object
    */

  def getRelatedMemoryObjects(
                               rel: ErasedRelationAttributes,
                               tree: MemoryTree
                             ): MemoryEither[Set[(MemoryObject, MemoryObject)]] = {
    val related = getRelated(rel.name)
    EitherOps
      .sequence(related.map(o => tree.findObj(rel.to, o)))
      .map(relatedObjects => relatedObjects.collect{case Some(v) => v}.map((this, _)))
  }

  /**
    * Get all objects related by a given relation to this object
    */
  def getRevRelated(relationName: RelationName): Set[DBObject] = revRelations.getOrElse(relationName, Set())

  /**
    * get all MemoryObject s related by a given relation to this object
    */

  def getRevRelatedMemoryObjects(
                               rel: ErasedRelationAttributes,
                               tree: MemoryTree
                             ): MemoryEither[Set[(MemoryObject, MemoryObject)]] = {
    val related = getRevRelated(rel.name)
    EitherOps
      .sequence(related.map(o => tree.findObj(rel.from, o)))
      .map(relatedObjects => relatedObjects.collect{case Some(v) => v}.map((this, _)))
  }

  /**
    * Pretty print
    */
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

  /**
    * Helper class
    */
  private[MemoryObject] case class Hashable(value: DBObject, name: TableName)
}