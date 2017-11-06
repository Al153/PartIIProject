package impl

import core.backend.common._
import core.error.E
import core.intermediate.unsafe._
import core.schema.TableName
import core.utils._

import scalaz._

/**
  * Created by Al on 20/10/2017.
  *
  * An in-memory database executor
  */
package object memory extends {
  type MemoryTree = Map[TableName, MemoryTable]
  type RelatedPair = (MemoryObject, MemoryObject)

  implicit class MemoryTreeOps(memoryTree: MemoryTree) {
    def findPattern(findable: UnsafeFindable): E \/ Vector[MemoryObject] = for {
      table <- memoryTree.getOrError(findable.tableName, MissingTableName(findable.tableName))
      res <- table.find(findable)
    } yield res

    def findObj(tableName: TableName, obj: DBObject): E \/ Option[MemoryObject] = for {
      table <- memoryTree.getOrError(tableName, MissingTableName(tableName))
      res = table.find(obj)
    } yield res
  }
}
