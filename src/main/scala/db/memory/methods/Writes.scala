package db.memory.methods

import core.error.E
import db.common.{DBObject, MissingTableName}
import db.memory.MemoryTree
import schema.{RelationName, TableName}
import utils._

import scalaz.\/

trait Writes {
  def write[A](t: MemoryTree, tableName1: TableName, memoryObject1: DBObject, relationName: RelationName, tableName2: TableName, memoryObject2: DBObject): E \/ MemoryTree = {
    if (tableName1 != tableName2) { // different behaviour
      for {
        table1 <- t.getOrError(tableName1, MissingTableName(tableName1))
        table2 <- t.getOrError(tableName2, MissingTableName(tableName2))

        o1 <- table1.findOrWrite(memoryObject1)
        updatedO1 =  o1.addRelation(relationName, memoryObject2)
        o2 <- table2.findOrWrite(memoryObject2)
        updatedO2 = o2.addReverseRelation(relationName, memoryObject1)
        res = t + (tableName1 -> table1.insert(updatedO1), tableName2 -> table2.insert(updatedO2))
      } yield res
    } else { // if same table need to add both to table
      writeSelfRelation(t, tableName1, memoryObject1, relationName, memoryObject2)
    }
  }

  private def writeSelfRelation(t: MemoryTree, tableName1: TableName, memoryObject1: DBObject, relationName: RelationName, memoryObject2: DBObject) =
    for {
      table1 <- t.getOrError(tableName1, MissingTableName(tableName1))

      o1 <- table1.findOrWrite(memoryObject1)
      updatedO1 =  o1.addRelation(relationName, memoryObject2)
      o2 <- table1.findOrWrite(memoryObject2)
      updatedO2 = o2.addReverseRelation(relationName, memoryObject1)

      res = t + (tableName1 -> table1.insert(updatedO1).insert(updatedO2))
    } yield res

}