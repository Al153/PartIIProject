package core.backend.intermediate

import core.backend.common._

import scalaz.Scalaz._
import scalaz.\/

/**
  * Created by Al on 17/10/2017.
  *
  * Typeclass for objects that can be converted to/from DBCells
  */

trait Storeable[T] {
  /**
    * @return the appropriate schema component that a [[T]] translates to
    */
  def SchemaComponent: SchemaComponent

  /**
    * convert a [[T]] to a [[DBCell]]
    */
  def toDBCell(t: T): DBCell

  /**
    * Try to convert a [[DBCell]] to a [[T]]
    */
  def get(dBCell: DBCell): ExtractError \/ T
}
object Storeable {
  implicit val storeableString = new Storeable[String] {
    override def SchemaComponent: SchemaComponent = StringCell
    override def toDBCell(s: String): DBCell = DBString(s)
    override def get(dBCell: DBCell): \/[SchemaMismatch, String] = dBCell match {
      case DBString(s) => s.right
      case _ => SchemaMismatch(SchemaComponent, dBCell).left
    }
  }
  implicit val storeableInt = new Storeable[Int] {
    override def SchemaComponent: SchemaComponent = IntCell
    override def toDBCell(i: Int): DBCell = DBInt(i)
    override def get(dBCell: DBCell): \/[SchemaMismatch, Int] = dBCell match {
      case DBInt(i) => i.right
      case _ => SchemaMismatch(SchemaComponent, dBCell).left
    }
  }
  implicit val storeableBoolean = new Storeable[Boolean] {
    override def SchemaComponent: SchemaComponent = BoolCell
    override def toDBCell(b: Boolean): DBCell = DBBool(b)
    override def get(dBCell: DBCell): \/[SchemaMismatch, Boolean] = dBCell match {
      case DBBool(b) => b.right
      case _ => SchemaMismatch(SchemaComponent, dBCell).left
    }
  }
  implicit val storeableDouble = new Storeable[Double] {
    override def SchemaComponent = DoubleCell
    override def toDBCell(d: Double): DBCell = DBDouble(d)
    override def get(dBCell: DBCell): \/[SchemaMismatch, Double] = dBCell match {
      case DBDouble(d) => d.right
      case _ => SchemaMismatch(SchemaComponent, dBCell).left
    }
  }
}