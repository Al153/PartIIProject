package schema

import core.intermediate.unsafe.SchemaObjectErased
import db.common._
import db.interfaces.Extractor

import scalaz.\/
import scalaz._
import Scalaz._

/**
  * Created by Al on 17/10/2017.
  */

sealed trait SchemaObject[A] extends Extractor[A] {
  def getSchemaComponents: List[SchemaComponent]
  def generalPattern: Pattern[A]
  def findable(a: A): Findable[A]
  def name: TableName

  final def erased: SchemaObjectErased = SchemaObjectErased(name, getSchemaComponents)


}

trait SchemaObject0[A] extends SchemaObject[A] {
  def construct(): A
  override def name: TableName
  def toTuple(a: A): DBTuple0[A]

  final def fromTuple(tuple0: DBTuple0[A]): A = construct()
  final def pattern: Pattern0[A] = Pattern0[A](name)
  final def generalPattern: Pattern[A] = pattern
  final override def getSchemaComponents: List[SchemaComponent] = pattern.toSchemaComponent
  final override def findable(a: A): Findable[A] = toTuple(a)
  final override def fromRow(row: DBObject): ExtractError \/ A = row match {
    case DBRow(fields) =>
      if (fields.isEmpty) construct().right
      else LengthMismatch().left
  }
}

abstract class SchemaObject1[A, A1](implicit s1: Storeable[A1]) extends SchemaObject[A] {
  def construct(a1: A1): A
  override def name: TableName
  def toTuple(a: A): DBTuple1[A, A1]

  final def fromTuple(tuple1: DBTuple1[A, A1]): A = construct(tuple1.a1)
  final def generalPattern: Pattern[A] = pattern
  final def pattern: Pattern1[A, A1] = Pattern1[A, A1](name, None)
  final override def getSchemaComponents: List[SchemaComponent] = pattern.toSchemaComponent
  final override def findable(a: A): Findable[A] = toTuple(a)
  final override def fromRow(row: DBObject): ExtractError \/ A = {
    val DBRow(fields) = row
    if (fields.length == 1) {
      s1.get(fields(0)).map(construct)
    } else LengthMismatch().left
  }
}

abstract class SchemaObject2[A, A1, A2](implicit s1: Storeable[A1], s2: Storeable[A2]) extends SchemaObject[A] {
  def construct(a1: A1, a2: A2): A
  override def name: TableName
  def toTuple(a: A): DBTuple2[A, A1, A2]

  final def fromTuple(tuple2: DBTuple2[A, A1, A2]): A = construct(tuple2.a1, tuple2.a2)
  final def generalPattern: Pattern[A] = pattern
  final def pattern: Pattern2[A, A1, A2] = Pattern2[A, A1, A2](name, None, None)
  final override def getSchemaComponents: List[SchemaComponent] = pattern.toSchemaComponent
  final override def findable(a: A): Findable[A] = toTuple(a)

  final override def fromRow(row: DBObject): ExtractError \/ A = {
    val DBRow(fields) = row
    if (fields.length == 2) {
      for {
        a1 <- s1.get(fields(0))
        a2 <- s2.get(fields(1))
      } yield construct(a1, a2)
    } else LengthMismatch().left
  }
}

abstract class SchemaObject3[A, A1, A2, A3](implicit s1: Storeable[A1], s2: Storeable[A2], s3: Storeable[A3]) extends SchemaObject[A] {
  def construct(a1: A1, a2: A2, a3: A3): A
  def name: TableName
  def toTuple(a: A): DBTuple3[A, A1, A2, A3]

  final def fromTuple(tuple3: DBTuple3[A, A1, A2, A3]): A = construct(tuple3.a1, tuple3.a2, tuple3.a3)
  final def generalPattern: Pattern[A] = pattern
  final def pattern: Pattern3[A, A1, A2, A3] = Pattern3[A, A1, A2, A3](name, None, None, None)
  final override def getSchemaComponents: List[SchemaComponent] = pattern.toSchemaComponent
  final override def findable(a: A): Findable[A] = toTuple(a)
  final override def fromRow(row: DBObject): ExtractError \/ A = {
    val DBRow(fields) = row
    if (fields.length == 3) {
      for {
        a1 <- s1.get(fields(0))
        a2 <- s2.get(fields(1))
        a3 <- s3.get(fields(3))
      } yield construct(a1, a2, a3)
    } else LengthMismatch().left
  }
}