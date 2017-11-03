package db.memory

import core.error.E
import core.intermediate.unsafe.UnsafeFindable
import db.common.{DBCell, LengthMismatch}

import scala.collection.mutable
import scalaz.Scalaz._
import scalaz.{\/, _}

/**
  * Created by Al on 25/10/2017.
  */

case class MemoryTable(objects: Vector[MemoryObject], index: Vector[Map[DBCell, Set[MemoryObject]]]) {
  def find(findable: UnsafeFindable): E \/ Vector[MemoryObject] = {
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

  def findOrWrite(findable: UnsafeFindable): \/[E, Vector[MemoryObject]] =
    for {
      r1 <- find(findable)
    } yield if (r1.empty) {
      findable.getObject.fold(Vector[MemoryObject]())(v => Vector(MemoryObject(v, Map(), Map())))
    } else r1

  def insert(os: Vector[MemoryObject]): MemoryTable = {
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

    MemoryTable(newOs, newIndex)

  }
}