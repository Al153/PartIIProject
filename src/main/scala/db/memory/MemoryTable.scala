package db.memory

import core.error.E
import core.intermediate.unsafe.{SchemaObjectErased, UnsafeFindable}
import db.common.{DBCell, DBObject, LengthMismatch}
import schema.TableName

import scala.collection.mutable
import utils._

import scalaz.Scalaz._
import scalaz.{\/, _}

/**
  * Created by Al on 25/10/2017.
  */

case class MemoryTable(objects: Map[DBObject, MemoryObject], index: Vector[Map[DBCell, Set[MemoryObject]]], name: TableName) {
  def find(findable: UnsafeFindable): E \/ Vector[MemoryObject] = {
    val pattern = findable.pattern
    if (pattern.length != index.length) LengthMismatch().left
    else {
      pattern.zip(index).foldLeft(objects.values.toVector.right[E]){
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

  def findOrWrite(findable: DBObject): \/[E, MemoryObject] =
    find(findable).getOrElse(MemoryObject(findable, name, Map(), Map())).right[E]

  def  insert(o: MemoryObject): MemoryTable = {
    // todo: need to look up the value first, potentially updating
    if (o.value in objects){
      // need to replace updated value in each table
      val newObjects = objects + (o.value -> o)
      val newIndex = for { // todo
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

  // create an empty table based on a schema
  def apply(so: SchemaObjectErased): MemoryTable = {
    val objects = Map[DBObject, MemoryObject]()
    val index = so.schemaComponents.map(_ => Map[DBCell, Set[MemoryObject]]())

    new MemoryTable(objects, index, so.name)
  }
}