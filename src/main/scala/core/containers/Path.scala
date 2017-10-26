package core.containers

import db.common.DBObject
import schema.SchemaObject

/**
  * Created by Al on 09/10/2017.
  */
abstract class Path[A](implicit sa: SchemaObject[A]) {
  def getStart: A
  def getEnd: A
  def getSteps: Vector[(A, A)] // get all steps in the path. Todo: can we get more information out here
}

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