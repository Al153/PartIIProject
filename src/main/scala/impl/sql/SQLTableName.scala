package impl.sql

import core.schema.TableName


/**
  * A tablename type that is used to make sure that tablenames don't clash with the views table

  */
case class SQLTableName private (s: String)

object SQLTableName {
  def apply(tn: TableName): SQLTableName = new SQLTableName("USERSPACE_" + tn.value)

  val viewsTable = new SQLTableName("VIEWS_ID")
}