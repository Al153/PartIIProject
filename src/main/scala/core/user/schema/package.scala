package core.user

import core.backend.common.{DBCell, DBObject}
import core.backend.intermediate.unsafe.ErasedFindable
import core.backend.intermediate.{SchemaComponent, Storeable}

import scalaz.Scalaz._

/**
  * Created by Al on 17/10/2017.
  * Single stop import for all the schema related objects
  */
package object schema {


  /**
    * Something that can be looked up in the database (not necessarily complete data)
    * @tparam A - used to typecheck constructions using the findable
    */

  sealed trait Findable[A] {
    /**
      * The pattern that is used during lookup
      * @return
      */
    def pattern: Vector[Option[DBCell]]

    /**
      * The tablename associated with the finable
      * @return
      */
    def tableName: TableName

    /**
      * Convert the findable to an erased equivalent
      * @return
      */
    def getUnsafe = ErasedFindable(pattern, tableName)
    def isEmpty: Boolean = pattern.forall(_.isEmpty)
  }

  /**
    * A findable that is guaranteed to be complete
    * Hence it also provides a midway point for conversion to a DBObject
    */

  sealed trait DBTuple[A] extends Findable[A] {
    def toDBObject: DBObject

    override val isEmpty: Boolean = false
  }

  /**
    * Distinct Subsclass for each number of parameters
    */
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

  case class DBTuple4[Res, A1, A2, A3, A4](n: TableName, a1: A1, a2: A2, a3: A3, a4: A4)(implicit s1: Storeable[A1], s2: Storeable[A2], s3: Storeable[A3], s4: Storeable[A4]) extends DBTuple[Res]{
    override def toDBObject = DBObject(Vector(s1.toDBCell(a1), s2.toDBCell(a2), s3.toDBCell(a3), s4.toDBCell(a4)))
    override def pattern: Vector[Option[DBCell]] = Vector(s1.toDBCell(a1), s2.toDBCell(a2), s3.toDBCell(a3), s4.toDBCell(a4)).map(_.some)
    override def tableName: TableName = n
  }


  /**
    * A pattern is a Findable that may or may not be full.
    *
    * Has a separate subclass for each possible number of arguments
    */

  sealed trait Pattern[A] extends Findable[A] {
    def toSchemaComponent: Vector[SchemaComponent]
  }

  case class Pattern0[Res](n: TableName) extends Pattern[Res] {
    override def toSchemaComponent: Vector[SchemaComponent] = Vector()
    override def tableName: TableName = n
    override def pattern: Vector[Option[DBCell]] = Vector()
  }

  case class Pattern1[Res, A1](n: TableName, a1: Option[A1])(implicit s1: Storeable[A1]) extends Pattern[Res] {
    override def toSchemaComponent: Vector[SchemaComponent] = Vector(s1.SchemaComponent)
    override def tableName: TableName = n
    override def pattern: Vector[Option[DBCell]] = Vector(a1.map(s1.toDBCell))
  }

  case class Pattern2[Res, A1, A2](n: TableName, a1: Option[A1], a2: Option[A2])(implicit s1: Storeable[A1], s2: Storeable[A2]) extends Pattern[Res] {
    override def toSchemaComponent: Vector[SchemaComponent] = Vector(s1.SchemaComponent, s2.SchemaComponent)
    override def tableName: TableName = n
    override def pattern: Vector[Option[DBCell]] = Vector(a1.map(s1.toDBCell), a2.map(s2.toDBCell))
  }

  case class Pattern3[Res, A1, A2, A3](n: TableName, a1: Option[A1], a2: Option[A2], a3: Option[A3])(implicit s1: Storeable[A1], s2: Storeable[A2], s3: Storeable[A3]) extends Pattern[Res] {
    override def toSchemaComponent: Vector[SchemaComponent] = Vector(s1.SchemaComponent, s2.SchemaComponent, s3.SchemaComponent)
    override def tableName: TableName = n
    override def pattern: Vector[Option[DBCell]] = Vector(a1.map(s1.toDBCell), a2.map(s2.toDBCell), a3.map(s3.toDBCell))
  }

  case class Pattern4[Res, A1, A2, A3, A4](n: TableName, a1: Option[A1], a2: Option[A2], a3: Option[A3], a4: Option[A4])(implicit s1: Storeable[A1], s2: Storeable[A2], s3: Storeable[A3], s4: Storeable[A4]) extends Pattern[Res] {
    override def toSchemaComponent: Vector[SchemaComponent] = Vector(s1.SchemaComponent, s2.SchemaComponent, s3.SchemaComponent, s4.SchemaComponent)
    override def tableName: TableName = n
    override def pattern: Vector[Option[DBCell]] = Vector(a1.map(s1.toDBCell), a2.map(s2.toDBCell), a3.map(s3.toDBCell), a4.map(s4.toDBCell))
  }
}

