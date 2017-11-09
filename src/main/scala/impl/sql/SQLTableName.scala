package impl.sql

import core.schema.{RelationName, TableName}
import core.utils._


/**
  * A tablename type that is used to make sure that tablenames don't clash with the views table

  */
class SQLTableName private (val s: String) extends AnyVal

object SQLTableName {
  def apply(tn: TableName): SQLTableName = new SQLTableName("USERSPACE_" + tn.value.strip)

  def apply(r: RelationName): SQLTableName = new SQLTableName("REL_"+ r.id.strip)

  val viewsTable = new SQLTableName("VIEWS_ID")


}