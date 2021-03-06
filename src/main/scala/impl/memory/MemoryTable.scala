package impl.memory

import core.backend.common.{DBCell, DBObject, LengthMismatch}
import core.backend.intermediate.unsafe.ErasedFindable
import core.user.schema.{SchemaObject, TableName}
import core.utils._
import impl.memory.errors.MemoryExtractError

import scalaz.Scalaz._

/**
  * Created by Al on 25/10/2017.
  *
  * Class containing an in memory lookup table.
  * This allows the lookup of whole DBObjects and indexing by individual DBCells
  */

case class MemoryTable(objects: Map[DBObject, MemoryObject], index: Vector[Map[DBCell, Set[MemoryObject]]], name: TableName) {
  /**
    *    lookup a findable in the table
    */

  def find(findable: ErasedFindable): MemoryEither[Set[MemoryObject]] = {
    val pattern = findable.pattern
    if (pattern.length != index.length)
      MemoryExtractError(LengthMismatch(pattern.length, index.length)).left
    else {
      bigIntersection(objects.values.toSet, pattern.zip(index).collect {case (Some(cell), map) => map.getOrElse(cell, Set())}).right
    }
  }

  /**
    * Lookup a DBObject in the table
    */
  def find(value: DBObject): Option[MemoryObject] = objects.get(value)

  /**
    * Lookup a DBObject in the table, creating one if it doesn't exist, and returning the MemoryObject
    */

  def findOrWrite(findable: DBObject): MemoryEither[MemoryObject] =
    MemoryEither(find(findable).getOrElse(MemoryObject(findable, name, Map(), Map())))

  /**
    * Inserts a memory object to the table
    */
  def insert(o: MemoryObject): MemoryTable = {
    if (o.value in objects){
      // need to replace updated value in each table
      val newObjects = objects + (o.value -> o)
      val newIndex = for {
        (cell, map) <- o.value.fields.zip(index)
      } yield map + (cell -> (map.getOrElse(cell, Set()) - o + o)) // since we remove by the hash value, then insert the updated version

      MemoryTable(newObjects, newIndex, name)

    } else {
      val newOs = objects + (o.value -> o)
      val newIndex = for {
        (cell, map) <- o.value.fields.zip(index)
      } yield map + (cell -> (map.getOrElse(cell, Set()) + o))

      MemoryTable(newOs, newIndex, name)
    }
  }
}

object MemoryTable {

  /**
    * create an empty table based on a core.user.schema
    */

  def apply(so: SchemaObject[_]): MemoryTable = {
    val objects = Map[DBObject, MemoryObject]()
    val index = so.schemaComponents.map(_ => Map[DBCell, Set[MemoryObject]]())

    new MemoryTable(objects, index, so.name)
  }
}