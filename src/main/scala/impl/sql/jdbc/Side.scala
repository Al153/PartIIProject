package impl.sql.jdbc

import impl.sql.names.SQLColumnName

/**
  * Simple trait hierarchy for a "Side Pointer"
  *
  * In any extract usage, etc, we need to know which side of a relation the value came from, in order to
  * use the right column aliases, etc
  */

sealed trait Side {
  /**
    * Give a way of getting columns of the right index
    */
  def columnName(i: Int): SQLColumnName

  /**
    * Give a way of getting hte id column of an object
    * @return
    */
  def getId: SQLColumnName
}
case object Left extends Side {
  override def columnName(i: Int): SQLColumnName = SQLColumnName.leftColumn(i)
  override def getId: SQLColumnName = SQLColumnName.leftId
}

case object Right extends Side {
  override def columnName(i: Int): SQLColumnName = SQLColumnName.rightColumn(i)
  override def getId: SQLColumnName = SQLColumnName.rightId
}

case object Single extends Side {
  override def columnName(i: Int): SQLColumnName = SQLColumnName.column(i)
  override def getId: SQLColumnName = SQLColumnName.objId
}