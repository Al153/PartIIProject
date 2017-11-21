package core.intermediate.unsafe

import core.schema.TableName

/**
  * Created by Al on 24/10/2017.
  *
  * Type-erased find pair datastructure
  * Need to limit construction to prevent end user-level programmers from constructing them
  */
sealed trait UnsafeFindPair {
  def leftMostTable: TableName // this is the table name at the bottom left most of the tree. Used for zero length queries
  def rightMostTable: TableName // this is the table name at the right most end of the tree. ditto. used to grab data from right end of the table
}
case class USAnd(l: UnsafeFindPair, r: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = l.leftMostTable
  override def rightMostTable: TableName = l.rightMostTable
}
case class USAndSingle(l: UnsafeFindPair, r: UnsafeFindSingle) extends UnsafeFindPair {
  override def leftMostTable: TableName = l.leftMostTable
  override def rightMostTable: TableName = l.rightMostTable
}
case class USAtleast(n: Int, rel: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = rel.leftMostTable
  override def rightMostTable: TableName = rel.rightMostTable
}
case class USBetween(low: Int, high: Int, rel: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = rel.leftMostTable
  override def rightMostTable: TableName = rel.rightMostTable
}
case class USChain(l: UnsafeFindPair, r: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = l.leftMostTable
  override def rightMostTable: TableName = r.rightMostTable
}
case class USDistinct(r: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = r.leftMostTable
  override def rightMostTable: TableName = r.rightMostTable
}
case class USExactly(n: Int, rel: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = rel.leftMostTable
  override def rightMostTable: TableName = rel.rightMostTable
}
case class USId(tableName: TableName) extends UnsafeFindPair {
  override def leftMostTable: TableName = tableName
  override def rightMostTable: TableName = tableName
}
case class USNarrow(left: UnsafeFindPair, f: UnsafeFindable) extends UnsafeFindPair {
  override def leftMostTable: TableName = left.leftMostTable
  override def rightMostTable: TableName = left.rightMostTable
}
case class USOr(left: UnsafeFindPair, right: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = left.leftMostTable
  override def rightMostTable: TableName = left.rightMostTable
}
case class USRel(r: ErasedRelationAttributes) extends UnsafeFindPair {
  override def leftMostTable: TableName = r.from
  override def rightMostTable: TableName = r.to
}
case class USRevRel(r: ErasedRelationAttributes) extends UnsafeFindPair {
  override def leftMostTable: TableName = r.to
  override def rightMostTable: TableName = r.from
}
case class USUpto(n: Int, rel: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = rel.leftMostTable
  override def rightMostTable: TableName = rel.rightMostTable
}

