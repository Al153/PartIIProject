package core.backend.intermediate.unsafe

import core.user.schema.TableName

/**
  * Created by Al on 25/10/2017.
  *
  * Type-erased equivalent of the [[core.backend.intermediate.FindSingle]] ADT
  */
sealed trait UnsafeFindSingle {
  def table: TableName
}
case class USFind private [intermediate] (pattern: ErasedFindable) extends UnsafeFindSingle {
  override def table: TableName = pattern.tableName
}
case class USFrom private [intermediate] (start: UnsafeFindSingle, rel: UnsafeFindPair) extends UnsafeFindSingle {
  override def table: TableName = rel.rightMostTable
}
case class USNarrowS private [intermediate] (start: UnsafeFindSingle, pattern: ErasedFindable) extends UnsafeFindSingle {
  override def table: TableName = pattern.tableName
}