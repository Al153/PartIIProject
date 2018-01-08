package core.user.schema

import core.backend.common._
import core.backend.intermediate.unsafe.SchemaObjectErased
import core.backend.intermediate.{SchemaComponent, Storeable}

import scalaz.Scalaz._
import scalaz.\/

/**
  * Created by Al on 17/10/2017.
  */

sealed trait SchemaObject[A] {
  def getSchemaComponents: Vector[SchemaComponent]
  def generalPattern: Pattern[A]
  def findable(a: A): DBTuple[A]
  def tableName: TableName
  def fromRow(row: DBObject): ExtractError \/ A

  final def erased: SchemaObjectErased = SchemaObjectErased(tableName, getSchemaComponents)
  final def getDBObject(a: A): DBObject = findable(a).toDBObject
  final def length: Int = getSchemaComponents.length


}

trait SchemaObject0[A] extends SchemaObject[A] {
  def construct(): A
  override def tableName: TableName
  def toTuple(a: A): DBTuple0[A]

  final def fromTuple(tuple0: DBTuple0[A]): A = construct()
  final def pattern: Pattern0[A] = Pattern0[A](tableName)
  final def generalPattern: Pattern[A] = pattern
  final override def getSchemaComponents: Vector[SchemaComponent] = pattern.toSchemaComponent
  final override def findable(a: A): DBTuple[A] = toTuple(a)
  final override def fromRow(row: DBObject): ExtractError \/ A =
    if (row.fields.isEmpty) construct().right else LengthMismatch(row.fields.length, 0).left

}

abstract class SchemaObject1[A, A1](implicit s1: Storeable[A1]) extends SchemaObject[A] {
  def construct(a1: A1): A
  override def tableName: TableName
  def toTuple(a: A): DBTuple1[A, A1]

  final def fromTuple(tuple1: DBTuple1[A, A1]): A = construct(tuple1.a1)
  final def generalPattern: Pattern[A] = pattern
  final def pattern: Pattern1[A, A1] = Pattern1[A, A1](tableName, None)
  final override def getSchemaComponents: Vector[SchemaComponent] = pattern.toSchemaComponent
  final override def findable(a: A): DBTuple[A] = toTuple(a)
  final override def fromRow(row: DBObject): ExtractError \/ A =
    if (row.fields.length == 1) {
      s1.get(row.fields(0)).map(construct)
    } else LengthMismatch(row.fields.length, 1).left

  final def pattern(o1: Option[A1]): Pattern1[A, A1] = Pattern1[A, A1](tableName, o1)

}

abstract class SchemaObject2[A, A1, A2](implicit s1: Storeable[A1], s2: Storeable[A2]) extends SchemaObject[A] {
  def construct(a1: A1, a2: A2): A
  override def tableName: TableName
  def toTuple(a: A): DBTuple2[A, A1, A2]

  final def fromTuple(tuple2: DBTuple2[A, A1, A2]): A = construct(tuple2.a1, tuple2.a2)
  final def generalPattern: Pattern[A] = pattern
  final def pattern: Pattern2[A, A1, A2] = Pattern2[A, A1, A2](tableName, None, None)
  final override def getSchemaComponents: Vector[SchemaComponent] = pattern.toSchemaComponent
  final override def findable(a: A): DBTuple[A] = toTuple(a)

  final override def fromRow(row: DBObject): ExtractError \/ A =
    if (row.fields.length == 2) {
      for {
        a1 <- s1.get(row.fields(0))
        a2 <- s2.get(row.fields(1))
      } yield construct(a1, a2)
    } else LengthMismatch(row.fields.length, 2).left

  final def pattern(o1: Option[A1], o2: Option[A2]): Pattern2[A, A1, A2] = Pattern2[A, A1, A2](tableName, o1, o2)
}

abstract class SchemaObject3[A, A1, A2, A3](implicit s1: Storeable[A1], s2: Storeable[A2], s3: Storeable[A3]) extends SchemaObject[A] {
  def construct(a1: A1, a2: A2, a3: A3): A
  def tableName: TableName
  def toTuple(a: A): DBTuple3[A, A1, A2, A3]

  final def fromTuple(tuple3: DBTuple3[A, A1, A2, A3]): A = construct(tuple3.a1, tuple3.a2, tuple3.a3)
  final def generalPattern: Pattern[A] = pattern
  final def pattern: Pattern3[A, A1, A2, A3] = Pattern3[A, A1, A2, A3](tableName, None, None, None)
  final override def getSchemaComponents: Vector[SchemaComponent] = pattern.toSchemaComponent
  final override def findable(a: A): DBTuple[A] = toTuple(a)
  final override def fromRow(row: DBObject): ExtractError \/ A =
    if (row.fields.length == 3) {
      for {
        a1 <- s1.get(row.fields(0))
        a2 <- s2.get(row.fields(1))
        a3 <- s3.get(row.fields(2))
      } yield construct(a1, a2, a3)
    } else LengthMismatch(row.fields.length, 3).left

  final def pattern(o1: Option[A1], o2: Option[A2], o3: Option[A3]): Pattern3[A, A1, A2, A3] = Pattern3[A, A1, A2, A3](tableName, o1, o2, o3)
}


abstract class SchemaObject4[A, A1, A2, A3, A4](implicit s1: Storeable[A1], s2: Storeable[A2], s3: Storeable[A3], s4: Storeable[A4]) extends SchemaObject[A] {
  def construct(a1: A1, a2: A2, a3: A3, a4: A4): A
  def tableName: TableName
  def toTuple(a: A): DBTuple4[A, A1, A2, A3, A4]

  final def fromTuple(tuple4: DBTuple4[A, A1, A2, A3, A4]): A = construct(tuple4.a1, tuple4.a2, tuple4.a3, tuple4.a4)
  final def generalPattern: Pattern[A] = pattern
  final def pattern: Pattern4[A, A1, A2, A3, A4] = Pattern4[A, A1, A2, A3, A4](tableName, None, None, None, None)
  final override def getSchemaComponents: Vector[SchemaComponent] = pattern.toSchemaComponent
  final override def findable(a: A): DBTuple[A] = toTuple(a)
  final override def fromRow(row: DBObject): ExtractError \/ A =
    if (row.fields.length == 4) {
      for {
        a1 <- s1.get(row.fields(0))
        a2 <- s2.get(row.fields(1))
        a3 <- s3.get(row.fields(2))
        a4 <- s4.get(row.fields(3))
      } yield construct(a1, a2, a3, a4)
    } else LengthMismatch(row.fields.length, 4).left

  final def pattern(o1: Option[A1], o2: Option[A2], o3: Option[A3], o4: Option[A4]): Pattern4[A, A1, A2, A3, A4] = Pattern4[A, A1, A2, A3, A4](tableName, o1, o2, o3, o4)
}