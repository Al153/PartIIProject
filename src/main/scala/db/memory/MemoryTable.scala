package db.memory

import core.error.E
import core.intermediate.unsafe.{SchemaObjectErased, UnsafeFindable}
import db.common.{DBCell, LengthMismatch, SchemaMismatch}
import schema.SchemaDescription

import scalaz.\/
import scalaz._
import Scalaz._
import scala.collection.mutable

/**
  * Created by Al on 25/10/2017.
  */

sealed trait MemoryTable {
  def schema: SchemaObjectErased
  def objects: Vector[MemoryObject]

  def index: Vector[Map[DBCell, Set[MemoryObject]]] // to speed up lookup of values
  def find(findable: UnsafeFindable): E \/ Vector[MemoryObject] // fast lookup index
  def findOrWrite(findable: UnsafeFindable): E \/ Vector[MemoryObject] // lookup or return a new MemoryObject if its not present
  def insert(os: Vector[MemoryObject]): MemoryTable
}

case class MemoryTableImpl(objects: Vector[MemoryObject], index: Vector[Map[DBCell, Set[MemoryObject]]]) extends MemoryTable {
  override def find(findable: UnsafeFindable): E \/ Vector[MemoryObject] = {
    val pattern = findable.pattern
    if (pattern.length != index.length) LengthMismatch().left
    else {
      pattern.zip(index).foldLeft(objects.right[E]){
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

  override def findOrWrite(findable: UnsafeFindable): \/[E, Vector[MemoryObject]] =
    for {
      r1 <- find(findable)
    } yield if (r1.empty) {
      findable.getObject.fold(Vector[MemoryObject]())(v => Vector(MemoryObjectImpl(v, Map(), Map())))
    } else r1

  override def insert(os: Vector[MemoryObject]): MemoryTable = {
    val newOs = objects.union(os)
    val indexBuilder = for {
      i <- 0 to index.length
    } yield mutable.Map[DBCell, Set[MemoryObject]]()

    for (o <- os){
      for (
        (cell, map) <- o.value.fields.zip(indexBuilder)
      ){
        map += (cell -> (map.getOrElse(cell, Set[MemoryObject]()) + o))
      }
    }


    val newIndex = index.zip(indexBuilder).map {case (m1, m2) => m1 ++ m2.toMap}

    MemoryTableImpl(newOs, newIndex)

  }

  override def schema: SchemaObjectErased = ???
}