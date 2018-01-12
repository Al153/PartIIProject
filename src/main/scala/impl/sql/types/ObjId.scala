package impl.sql.types


/**
  * Simple value class for ObjIds
  */
case class ObjId(id: Long) extends AnyVal {
  override def toString: String = id.toString
}