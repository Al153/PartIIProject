package core.backend.intermediate.unsafe

import core.user.schema.TableName

/**
  * Created by Al on 24/10/2017.
  *
  * Type-erased equivalent versions of the [[core.backend.intermediate.FindPair]] ADT
  * Need to limit construction to prevent end user-level programmers from constructing them
  */
sealed trait UnsafeFindPair {
  def leftMostTable: TableName // this is the table name at the bottom left most of the tree. Used for zero length queries
  def rightMostTable: TableName // this is the table name at the right most end of the tree. ditto. used to grab data from right end of the table
}
case class USAnd private [intermediate] (l: UnsafeFindPair, r: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = l.leftMostTable
  override def rightMostTable: TableName = l.rightMostTable
}
case class USAndRight private [intermediate] (l: UnsafeFindPair, r: UnsafeFindSingle) extends UnsafeFindPair {
  override def leftMostTable: TableName = l.leftMostTable
  override def rightMostTable: TableName = l.rightMostTable
}

case class USAndLeft private [intermediate] (l: UnsafeFindPair, r: UnsafeFindSingle) extends UnsafeFindPair {
  override def leftMostTable: TableName = l.leftMostTable
  override def rightMostTable: TableName = l.rightMostTable
}

case class USAtleast private [intermediate] (n: Int, rel: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = rel.leftMostTable
  override def rightMostTable: TableName = rel.rightMostTable
}
case class USBetween private [intermediate] (low: Int, high: Int, rel: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = rel.leftMostTable
  override def rightMostTable: TableName = rel.rightMostTable
}
case class USChain private [intermediate] (l: UnsafeFindPair, r: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = l.leftMostTable
  override def rightMostTable: TableName = r.rightMostTable
}
case class USDistinct private [intermediate] (r: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = r.leftMostTable
  override def rightMostTable: TableName = r.rightMostTable
}
case class USExactly private [intermediate] (n: Int, rel: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = rel.leftMostTable
  override def rightMostTable: TableName = rel.rightMostTable
}
case class USId private [intermediate] (tableName: TableName) extends UnsafeFindPair {
  override def leftMostTable: TableName = tableName
  override def rightMostTable: TableName = tableName
}

case class USOr private [intermediate] (left: UnsafeFindPair, right: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = left.leftMostTable
  override def rightMostTable: TableName = left.rightMostTable
}
case class USRel private [intermediate] (r: ErasedRelationAttributes) extends UnsafeFindPair {
  override def leftMostTable: TableName = r.from
  override def rightMostTable: TableName = r.to
}
case class USRevRel private [intermediate] (r: ErasedRelationAttributes) extends UnsafeFindPair {
  override def leftMostTable: TableName = r.to
  override def rightMostTable: TableName = r.from
}
case class USUpto private [intermediate] (n: Int, rel: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = rel.leftMostTable
  override def rightMostTable: TableName = rel.rightMostTable
}

