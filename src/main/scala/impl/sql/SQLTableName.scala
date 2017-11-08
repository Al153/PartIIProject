package impl.sql

import core.schema.{RelationName, TableName}
import core.utils._


/**
  * A tablename type that is used to make sure that tablenames don't clash with the views table

  */
case class SQLTableName private (s: String)

object SQLTableName {
  def apply(tn: TableName): SQLTableName = new SQLTableName("USERSPACE_" + tn.value.strip)

  def apply(r: RelationName): SQLTableName = new SQLTableName("REL_"+ r.id.strip)

  val viewsTable = new SQLTableName("VIEWS_ID")


}