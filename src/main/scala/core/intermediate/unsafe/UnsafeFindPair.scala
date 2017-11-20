package core.intermediate.unsafe

import core.schema.TableName

/**
  * Created by Al on 24/10/2017.
  *
  * Type-erased find pair datastructure
  * Need to limit construction to prevent end user-level programmers from constructing them
  */
sealed trait UnsafeFindPair {
  def leftMostTable: TableName // this is the table name at the bottom of the tree. Used for zero queries
}
case class USAnd(l: UnsafeFindPair, r: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = l.leftMostTable
}
case class USAndSingle(l: UnsafeFindPair, r: UnsafeFindSingle) extends UnsafeFindPair {
  override def leftMostTable: TableName = l.leftMostTable
}
case class USAtleast(n: Int, rel: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = rel.leftMostTable
}
case class USBetween(low: Int, high: Int, rel: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = rel.leftMostTable
}
case class USChain(l: UnsafeFindPair, r: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = l.leftMostTable
}
case class USDistinct(r: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = r.leftMostTable
}
case class USExactly(n: Int, rel: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = rel.leftMostTable
}
case class USId(tableName: TableName) extends UnsafeFindPair {
  override def leftMostTable: TableName = tableName
}
case class USNarrow(left: UnsafeFindPair, f: UnsafeFindable) extends UnsafeFindPair {
  override def leftMostTable: TableName = left.leftMostTable
}
case class USOr(left: UnsafeFindPair, right: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = left.leftMostTable
}
case class USRel(r: ErasedRelationAttributes) extends UnsafeFindPair {
  override def leftMostTable: TableName = r.from
}
case class USRevRel(r: ErasedRelationAttributes) extends UnsafeFindPair {
  override def leftMostTable: TableName = r.to
}
case class USUpto(n: Int, rel: UnsafeFindPair) extends UnsafeFindPair {
  override def leftMostTable: TableName = rel.leftMostTable
}

