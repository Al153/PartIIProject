package core.intermediate.unsafe

import db.common.{DBCell, DBObject}
import schema.TableName

import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 25/10/2017.
  */
case class UnsafeFindable(pattern: Vector[Option[DBCell]], tableName: TableName) {
  lazy val full: Boolean = pattern.forall(_.isDefined)

  // Build an object from the findable if it is possible to
  def getObject: Option[DBObject] = pattern.foldLeft(Vector[DBCell]().some){
    case (ov, oc) =>
      for {
        v <- ov
        c <- oc
      } yield v :+ c
  }.map(DBObject.apply)
}
