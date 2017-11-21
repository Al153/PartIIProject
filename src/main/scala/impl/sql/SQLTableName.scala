package impl.sql

import core.schema.{RelationName, TableName}
import core.utils._


/**
  * A tablename type that is used to make sure that tablenames don't clash with the views table

  */
sealed trait SQLTableName {
  def name: String

  override def toString: String = name
}

class ObjectTableName(in: TableName) extends SQLTableName {
  override val name: String = "USERSPACE_" + in.value.strip
}
class RelationTableName(r: RelationName) extends SQLTableName {
  override val name: String = "REL_" + r.id.strip
}
object ViewsTableName extends SQLTableName {
  override def name: String = "VIEWS_ID"
}

