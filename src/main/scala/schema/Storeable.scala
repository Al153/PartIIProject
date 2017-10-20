package schema

import db.common._
import db.interfaces.{ExtractError, SchemaMismatch}

import scalaz.\/
import scalaz.Scalaz._

/**
  * Created by Al on 17/10/2017.
  */
trait Storeable[T] {
  def schemaSummary: SchemaSummary
  def get(dBCell: DBCell): ExtractError \/ T
}
object Storeable {
  implicit val storeableString = new Storeable[String] {
    override def schemaSummary: SchemaSummary = StringCell

    override def get(dBCell: DBCell) = dBCell match {
      case DBString(s) => s.right
      case _ => SchemaMismatch(schemaSummary, ???).left
    }
  }
  implicit val storeableInt = new Storeable[Int] {
    override def schemaSummary: SchemaSummary = IntCell
    override def get(dBCell: DBCell) = dBCell match {
      case DBInt(i) => i.right
      case _ => SchemaMismatch(schemaSummary, ???).left
    }
  }
  implicit val storeableBoolean = new Storeable[Boolean] {
    override def schemaSummary: SchemaSummary = BoolCell
    override def get(dBCell: DBCell) = dBCell match {
      case DBBool(b) => b.right
      case _ => SchemaMismatch(schemaSummary, ???).left
    }
  }
  implicit val storeableDouble = new Storeable[Double] {
    override def schemaSummary = DoubleCell
    override def get(dBCell: DBCell) = dBCell match {
      case DBDouble(d) => d.right
      case _ => SchemaMismatch(schemaSummary, ???).left
    }
  }
}