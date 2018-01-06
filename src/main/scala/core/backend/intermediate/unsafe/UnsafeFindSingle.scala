package core.backend.intermediate.unsafe

import core.user.schema.TableName

/**
  * Created by Al on 25/10/2017.
  */
sealed trait UnsafeFindSingle {
  def table: TableName
}
case class USFind(pattern: UnsafeFindable) extends UnsafeFindSingle {
  override def table: TableName = pattern.tableName
}
case class USFrom(start: UnsafeFindSingle, rel: UnsafeFindPair) extends UnsafeFindSingle {
  override def table: TableName = rel.rightMostTable
}
case class USNarrowS(start: UnsafeFindSingle, pattern: UnsafeFindable) extends UnsafeFindSingle {
  override def table: TableName = pattern.tableName
}