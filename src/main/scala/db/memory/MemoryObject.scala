package db.memory

import core.intermediate.unsafe.UnsafeFindable
import db.common.DBObject
import schema.RelationName

/**
  * Created by Al on 25/10/2017.
  */
sealed trait MemoryObject {
  def relations: Map[RelationName, Set[UnsafeFindable]]
  def revRelations: Map[RelationName, Set[UnsafeFindable]]
  def value: DBObject

  def addRelation(relation: RelationName, that: UnsafeFindable): MemoryObject
  def addReverseRelation(relation: RelationName, that: UnsafeFindable): MemoryObject

  def getRelated(relationName: RelationName): Set[UnsafeFindable] = relations.getOrElse(relationName, Set())
  def getRevRelated(relationName: RelationName): Set[UnsafeFindable] = revRelations.getOrElse(relationName, Set())
}

case class MemoryObjectImpl(value: DBObject, relations: Map[RelationName, Set[UnsafeFindable]], revRelations: Map[RelationName, Set[UnsafeFindable]]) extends MemoryObject {
  override def addRelation(relation: RelationName, that: UnsafeFindable): MemoryObject =
    if (relations.contains(relation)) {
      MemoryObjectImpl(value, relations + (relation -> (relations(relation) + that)), revRelations)
    } else {
      MemoryObjectImpl(value, relations + (relation -> Set(that)), revRelations)
    }


  override def addReverseRelation(relation: RelationName, that: UnsafeFindable): MemoryObject =
    if (revRelations.contains(relation)) {
      MemoryObjectImpl(value, relations, revRelations+ (relation -> (revRelations(relation) + that)))
    } else {
      MemoryObjectImpl(value, relations, revRelations + (relation -> Set(that)))
    }
}