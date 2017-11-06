package impl.memory

import core.backend.common.DBObject
import core.intermediate.unsafe.ErasedPath

/**
  * Created by Al on 27/10/2017.
  */
case class MemoryPath private (p: Vector[MemoryObject]) {
  def toErasedPath = new ErasedPath {
    override def getSteps: Vector[(DBObject, DBObject)] = {
      val converted = p.map(_.value)
      converted match {
        case _ +: tail => converted.zip(tail)
        case _ => Vector()
      }
    }
    override def getStart: DBObject = getSteps.head._1
    override def getEnd: DBObject = getSteps.last._2
  }

  def +(m: MemoryObject): MemoryPath = MemoryPath(p :+ m)
  def getLast: MemoryObject = p.last
}

object MemoryPath {
  def apply(p: MemoryObject): MemoryPath = new MemoryPath(Vector(p))
}