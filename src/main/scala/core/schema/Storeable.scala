package core.schema

import core.backend.common._


import scalaz.\/
import scalaz.Scalaz._

/**
  * Created by Al on 17/10/2017.
  */

// todo: Can this be optimised out?
trait Storeable[T] {
  def SchemaComponent: SchemaComponent
  def toDBCell(t: T): DBCell
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