package core

import core.intermediate.unsafe.UnsafeFindable
import core.backend.common.{DBCell, DBObject}
import core.schema.TableName

import scalaz.Scalaz._

/**
  * Created by Al on 17/10/2017.
  *
  * testing out conversion from object to core.schema value
  */
package object schema {



  sealed trait Findable[A] { // Can be found in the database (not necessarily complete data)
    def pattern: Vector[Option[DBCell]]
    def tableName: TableName
    def getUnsafe = UnsafeFindable(pattern, tableName)
  }
  sealed trait Writeable[A] // can be written to the database (necessarily complete data)

  sealed trait DBTuple[A] extends Findable[A] with Writeable[A]{
    def toDBObject: DBObject
  }

  case class DBTuple0[Res](n: TableName) extends DBTuple[Res] {
    override def toDBObject = DBObject(Vector())
    override def pattern: Vector[Option[DBCell]] = Vector()
    override def tableName: TableName = n
  }

  case class DBTuple1[Res, A1](n: TableName, a1: A1)(implicit s1: Storeable[A1]) extends DBTuple[Res]{
    override def toDBObject = DBObject(Vector(s1.toDBCell(a1)))
    override def pattern: Vector[Option[DBCell]] = Vector(s1.toDBCell(a1)).map(_.some)
    override def tableName: TableName = n
  }

  case class DBTuple2[Res, A1, A2](n: TableName, a1: A1, a2: A2)(implicit s1: Storeable[A1], s2: Storeable[A2]) extends DBTuple[Res]{
    override def toDBObject = DBObject(Vector(s1.toDBCell(a1), s2.toDBCell(a2)))
    override def pattern: Vector[Option[DBCell]] = Vector(s1.toDBCell(a1), s2.toDBCell(a2)).map(_.some)
    override def tableName: TableName = n
  }

  case class DBTuple3[Res, A1, A2, A3](n: TableName, a1: A1, a2: A2, a3: A3)(implicit s1: Storeable[A1], s2: Storeable[A2], s3: Storeable[A3]) extends DBTuple[Res]{
    override def toDBObject = DBObject(Vector(s1.toDBCell(a1), s2.toDBCell(a2), s3.toDBCell(a3)))
    override def pattern: Vector[Option[DBCell]] = Vector(s1.toDBCell(a1), s2.toDBCell(a2), s3.toDBCell(a3)).map(_.some)
    override def tableName: TableName = n
  }

  sealed trait Pattern[A] extends Findable[A] {
    def toSchemaComponent: Vector[SchemaComponent]
  }

  case class Pattern0[Res](n: TableName) extends Pattern[Res] {
    override def toSchemaComponent: Vector[SchemaComponent] = Vector()
    def apply(): Pattern0[Res] = this
    override def tableName: TableName = n
    override def pattern: Vector[Option[DBCell]] = Vector()
  }

  case class Pattern1[Res, A1](n: TableName, a1: Option[A1])(implicit s1: Storeable[A1]) extends Pattern[Res] {
    override def toSchemaComponent: Vector[SchemaComponent] = Vector(s1.SchemaComponent)
    def apply(a: A1): Pattern1[Res, A1] = Pattern1(n, Some(a))
    override def tableName: TableName = n
    override def pattern: Vector[Option[DBCell]] = Vector(a1.map(s1.toDBCell))
  }

  case class Pattern2[Res, A1, A2](n: TableName, a1: Option[A1], a2: Option[A2])(implicit s1: Storeable[A1], s2: Storeable[A2]) extends Pattern[Res] {
    override def toSchemaComponent: Vector[SchemaComponent] = Vector(s1.SchemaComponent, s2.SchemaComponent)
    def apply(a1: Option[A1], a2: Option[A2]): Pattern2[Res, A1, A2] = Pattern2(n, a1, a2)
    override def tableName: TableName = n
    override def pattern: Vector[Option[DBCell]] = Vector(a1.map(s1.toDBCell), a2.map(s2.toDBCell))
  }

  case class Pattern3[Res, A1, A2, A3](n: TableName, a1: Option[A1], a2: Option[A2], a3: Option[A3])(implicit s1: Storeable[A1], s2: Storeable[A2], s3: Storeable[A3]) extends Pattern[Res] {
    override def toSchemaComponent: Vector[SchemaComponent] = Vector(s1.SchemaComponent, s2.SchemaComponent, s3.SchemaComponent)
    def apply(a1: Option[A1], a2: Option[A2], a3: Option[A3]): Pattern3[Res, A1, A2, A3] = Pattern3(n, a1, a2, a3)
    override def tableName: TableName = n
    override def pattern: Vector[Option[DBCell]] = Vector(a1.map(s1.toDBCell), a2.map(s2.toDBCell), a3.map(s3.toDBCell))
  }




  def ?[A](o: SchemaObject0[A]): Pattern0[A] = o.pattern
  def ?[A, A1](o: SchemaObject1[A, A1]): Pattern1[A, A1] = o.pattern
  def ?[A, A1, A2](o: SchemaObject2[A, A1, A2]): Pattern2[A, A1, A2] = o.pattern
  def ?[A, A1, A2, A3](o: SchemaObject3[A, A1, A2, A3]): Pattern3[A, A1, A2, A3] = o.pattern

  def ??[A](o: SchemaObject[A]): Pattern[A] = o.generalPattern

  def !![A](a: A)(implicit schemaObject: SchemaObject[A]): Findable[A] = schemaObject.findable(a)
}

