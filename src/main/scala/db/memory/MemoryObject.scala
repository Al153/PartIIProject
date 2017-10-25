package db.memory

import db.common.DBObject
import schema.RelationName

/**
  * Created by Al on 25/10/2017.
  */
sealed trait MemoryObject {
  def relations: Map[RelationName, Vector[MemoryObject]]
  def revRelations: Map[RelationName, Vector[MemoryObject]]
  def value: Vector[DBObject]
}