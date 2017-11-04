package db.memory

import core.intermediate.unsafe.UnsafeFindable
import db.common.DBObject
import schema.RelationName

/**
  * Created by Al on 25/10/2017.
  */

case class MemoryObject(value: DBObject, relations: Map[RelationName, Set[DBObject]], revRelations: Map[RelationName, Set[DBObject]]) {
  def addRelation(relation: RelationName, that: DBObject): MemoryObject =
    if (relations.contains(relation)) {
      MemoryObject(value, relations + (relation -> (relations(relation) + that)), revRelations)
    } else {
      MemoryObject(value, relations + (relation -> Set(that)), revRelations)
    }

  def addReverseRelation(relation: RelationName, that: DBObject): MemoryObject =
    if (revRelations.contains(relation)) {
      MemoryObject(value, relations, revRelations+ (relation -> (revRelations(relation) + that)))
    } else {
      MemoryObject(value, relations, revRelations + (relation -> Set(that)))
    }

  def getRelated(relationName: RelationName): Set[DBObject] = relations.getOrElse(relationName, Set())
  def getRevRelated(relationName: RelationName): Set[DBObject] = revRelations.getOrElse(relationName, Set())

  /**
    * Both equals and hashcode look only at the DBObject part of the object.
    * This is to make updating of tables easier
    * @return
    */

  override def hashCode(): Int = value.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case MemoryObject(v, _, _) => value == v
    case _ => false
  }

}