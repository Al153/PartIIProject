package impl.memory.methods

import core.backend.common.DBObject
import core.backend.intermediate.RelationName
import core.user.schema.TableName
import core.utils._
import impl.memory.errors.MemoryMissingTableName
import impl.memory.{MemoryEither, MemoryTree}

trait Writes {
  def write[A](
                t: MemoryTree, tableName1: TableName,
                memoryObject1: DBObject,
                relationName: RelationName,
                tableName2: TableName,
                memoryObject2: DBObject
              ): MemoryEither[MemoryTree] = {
    if (tableName1 != tableName2) { // different behaviour
      for {
        table1 <- t.getOrError(tableName1, MemoryMissingTableName(tableName1))
        table2 <- t.getOrError(tableName2, MemoryMissingTableName(tableName2))

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

  private def writeSelfRelation(
                                 t: MemoryTree,
                                 tableName1: TableName,
                                 memoryObject1: DBObject,
                                 relationName: RelationName,
                                 memoryObject2: DBObject
                               ): MemoryEither[MemoryTree] =
    for {
      table1 <- t.getOrError(tableName1, MemoryMissingTableName(tableName1))

      o1 <- table1.findOrWrite(memoryObject1)
      updatedO1 =  o1.addRelation(relationName, memoryObject2)
      o2 <- table1.findOrWrite(memoryObject2)
      updatedO2 = o2.addReverseRelation(relationName, memoryObject1)

      res = t + (tableName1 -> table1.insert(updatedO1).insert(updatedO2))
    } yield res

}