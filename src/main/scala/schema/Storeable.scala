package schema

/**
  * Created by Al on 17/10/2017.
  */
trait Storeable[T] {
  def schemaSummary: SchemaSummary
}
object Storeable {
  implicit val storeableString = new Storeable[String] {
    override def schemaSummary: SchemaSummary = StringCell
  }
  implicit val storeableInt = new Storeable[Int] {
    override def schemaSummary: SchemaSummary = IntCell
  }
  implicit val storeableBoolean = new Storeable[Boolean] {
    override def schemaSummary: SchemaSummary = BoolCell
  }
}