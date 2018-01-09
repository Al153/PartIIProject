package core.backend.intermediate.unsafe

import core.backend.common.{DBCell, DBObject}
import core.user.schema.TableName
import core.utils._

import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 25/10/2017.
  *
  * Erased version of a Findable[A]
  */
case class ErasedFindable(pattern: Vector[Option[DBCell]], tableName: TableName) {
  lazy val full: Boolean = pattern.forall(_.isDefined)

  override def toString: String = tableName.value + ":" + pattern.map(_.fold(" _ ")(_.toString).truncate(9)).mkString("|")

  def matches(value: DBObject): Boolean = pattern.zip(value.fields).forall {
    case (of, c) => of.fold(true)(_ == c)
  }
}
