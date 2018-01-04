package impl.memory

import core.backend.common.{DBCell, DBObject, LengthMismatch}
import core.intermediate.unsafe.{SchemaObjectErased, UnsafeFindable}
import core.schema.TableName
import core.utils._
import impl.memory.errors.MemoryExtractError

import scalaz.Scalaz._

/**
  * Created by Al on 25/10/2017.
  */

case class MemoryTable(objects: Map[DBObject, MemoryObject], index: Vector[Map[DBCell, Set[MemoryObject]]], name: TableName) {
  def find(findable: UnsafeFindable): MemoryEither[Vector[MemoryObject]] = {
    val pattern = findable.pattern
    if (pattern.length != index.length)
      MemoryExtractError(LengthMismatch(pattern.length, index.length)).left
    else {
      pattern.zip(index).foldLeft(MemoryEither(objects.values.toVector)){
        case (eos, (filter, map)) =>
          filter match {
            case None => eos
            case Some(value) =>
              eos.map(
                os => os.intersect(map.getOrElse(value, Set()).toSeq)
              )

          }
      }
    }
  }

  def find(value: DBObject): Option[MemoryObject] = objects.get(value)

  def findOrWrite(findable: DBObject): MemoryEither[MemoryObject] =
    MemoryEither(find(findable).getOrElse(MemoryObject(findable, name, Map(), Map())))

  def  insert(o: MemoryObject): MemoryTable = {
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

  // create an empty table based on a core.schema
  def apply(so: SchemaObjectErased): MemoryTable = {
    val objects = Map[DBObject, MemoryObject]()
    val index = so.schemaComponents.map(_ => Map[DBCell, Set[MemoryObject]]())

    new MemoryTable(objects, index, so.name)
  }
}