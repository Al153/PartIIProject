package db.memory

import core.intermediate.unsafe.UnsafeFindable
import db.common.DBObject
import schema.RelationName

/**
  * Created by Al on 25/10/2017.
  */

case class MemoryObject(value: DBObject, relations: Map[RelationName, Set[UnsafeFindable]], revRelations: Map[RelationName, Set[UnsafeFindable]]) {
  def addRelation(relation: RelationName, that: UnsafeFindable): MemoryObject =
    if (relations.contains(relation)) {
      MemoryObject(value, relations + (relation -> (relations(relation) + that)), revRelations)
    } else {
      MemoryObject(value, relations + (relation -> Set(that)), revRelations)
    }

  def addReverseRelation(relation: RelationName, that: UnsafeFindable): MemoryObject =
    if (revRelations.contains(relation)) {
      MemoryObject(value, relations, revRelations+ (relation -> (revRelations(relation) + that)))
    } else {
      MemoryObject(value, relations, revRelations + (relation -> Set(that)))
    }

  def getRelated(relationName: RelationName): Set[UnsafeFindable] = relations.getOrElse(relationName, Set())
  def getRevRelated(relationName: RelationName): Set[UnsafeFindable] = revRelations.getOrElse(relationName, Set())

}