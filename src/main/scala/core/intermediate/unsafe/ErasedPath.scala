package core.intermediate.unsafe

import db.common.DBObject

/**
  * Created by Al on 27/10/2017.
  */
trait ErasedPath {
  def getStart: DBObject
  def getEnd: DBObject
  def getSteps: Vector[(DBObject, DBObject)]
}

object ErasedPath {
  implicit class ErasedPathOps(left: ErasedPath) {
    def +(p: (DBObject, DBObject)): ErasedPath = :+(p)
    def :+(p: (DBObject, DBObject)): ErasedPath = new ErasedPath {
      override val getSteps: Vector[(DBObject, DBObject)] = left.getSteps :+ p
      override val getStart: DBObject = left.getStart
      override val getEnd: DBObject = p._2
    }

    def +:(p: (DBObject, DBObject)) = new ErasedPath {
      override val getSteps: Vector[(DBObject, DBObject)] = p +: left.getSteps
      override val getStart: DBObject = p._1
      override val getEnd: DBObject = left.getEnd
    }

    def ++(right: ErasedPath) = new ErasedPath {
      override val getSteps: Vector[(DBObject, DBObject)] = left.getSteps ++ right.getSteps
      override val getStart: DBObject = left.getStart
      override val getEnd: DBObject = right.getEnd
    }
  }

  def apply(pair: (DBObject, DBObject)): ErasedPath = new ErasedPath {
    override val getStart: DBObject = pair._1
    override val getEnd: DBObject = pair._2
    override val getSteps: Vector[(DBObject, DBObject)] = Vector(pair)
  }
}