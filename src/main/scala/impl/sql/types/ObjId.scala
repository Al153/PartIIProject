package impl.sql.types

case class ObjId(id: Long) extends AnyVal {
  override def toString: String = id.toString
}