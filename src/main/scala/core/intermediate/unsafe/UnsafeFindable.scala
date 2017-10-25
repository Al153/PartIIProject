package core.intermediate.unsafe

import db.common.DBCell
import schema.{Findable, TableName}

/**
  * Created by Al on 25/10/2017.
  */
case class UnsafeFindable(pattern: Vector[Option[DBCell]], tableName: TableName)

object UnsafeFindable {
  def apply[A](fa: Findable[A]): UnsafeFindable = new UnsafeFindable(fa.pattern, fa.tableName)
}
