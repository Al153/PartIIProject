package core.user.schema

import core.backend.common._
import core.backend.intermediate.{SchemaComponent, Storeable}

import scalaz.Scalaz._
import scalaz.\/

/**
  * Created by Al on 17/10/2017.
  *
  * A SchemaObject of a type is evidence that it can be converted to and from something in the database
  */

sealed trait SchemaObject[A] {
  /**
    * Get the schema components for objects of type [[A]] in order to generate Schema to store them
    */
  def schemaComponents: Vector[SchemaComponent]

  /**
    * Get an empty [[Findable]] of [[A]]
    */
  def any: Pattern[A]

  /**
    * Convert an [[A]] into a [[Findable]]
   */
  def findable(a: A): DBTuple[A]

  /**
    * Name of the Table to store values in
    */
  def name: TableName

  /**
    * Extract an [[A]] from a DBObject
    */

  def fromRow(row: DBObject): ExtractError \/ A

  /**
    * Convert an A to something that can be stored in the DB
    */

  final def getDBObject(a: A): DBObject = findable(a).toDBObject

  /**
    * Get the width of the schema for A
    */
  final def length: Int = schemaComponents.length
}

/**
  * Separate subclasses for each size of object
  */

trait SchemaObject0[A] extends SchemaObject[A] {
  /**
    * Need to define a constructor for an [[A]] from the components gained from the table
    */
  def construct(): A

  /**
    * Need to define the expected name of the table
    */
  override def name: TableName

  /**
    * Need to define how to convert an [[A]] into some storeable primitives
    */

  def toTuple(a: A): DBTuple0[A]

  /**
    * Empty Findable that lookups up all values of the type
    */

  final def any: Pattern[A] = Pattern0[A](name)


  /**
    * Get the schema components for objects of type [[A]] in order to generate Schema to store them
    */
  final override def schemaComponents: Vector[SchemaComponent] = any.toSchemaComponent

  /**
    * Convert an [[A]] into a [[Findable]]
    */
  final override def findable(a: A): DBTuple[A] = toTuple(a)

  /**
    * Extract an [[A]] from a DBObject
    */

  final override def fromRow(row: DBObject): ExtractError \/ A =
    if (row.fields.isEmpty) construct().right else LengthMismatch(row.fields.length, 0).left

  /**
    * Defines how to make a findable with certain fields
    */

  final def pattern: Pattern[A] = any

}

abstract class SchemaObject1[A, A1](implicit s1: Storeable[A1]) extends SchemaObject[A] {

  /**
    * Need to define a constructor for an [[A]] from the components gained from the table
    */

  def construct(a1: A1): A

  /**
    * Need to define the expected name of the table
    */
  override def name: TableName

  /**
    * Need to define how to convert an [[A]] into some storeable primitives
    */
  def toTuple(a: A): DBTuple1[A, A1]

  /**
    * Empty Findable that lookups up all values of the type
    */

  final def any: Pattern[A] = Pattern1[A, A1](name, None)


  /**
    * Get the schema components for objects of type [[A]] in order to generate Schema to store them
    */
  final override def schemaComponents: Vector[SchemaComponent] = any.toSchemaComponent

  /**
    * Convert an [[A]] into a [[Findable]]
    */
  final override def findable(a: A): DBTuple[A] = toTuple(a)

  /**
    * Extract an [[A]] from a DBObject
    */
  final override def fromRow(row: DBObject): ExtractError \/ A =
    if (row.fields.length == 1) {
      s1.get(row.fields(0)).map(construct)
    } else LengthMismatch(row.fields.length, 1).left


  /**
    * Defines how to make a findable with certain fields
    */

  final def pattern(o1: Option[A1]): Pattern1[A, A1] = Pattern1[A, A1](name, o1)

}

abstract class SchemaObject2[A, A1, A2](implicit s1: Storeable[A1], s2: Storeable[A2]) extends SchemaObject[A] {
  /**
    * Need to define a constructor for an [[A]] from the components gained from the table
    */
  def construct(a1: A1, a2: A2): A

  /**
    * Need to define the expected name of the table
    */
  override def name: TableName

  /**
    * Need to define how to convert an [[A]] into some storeable primitives
    */
  def toTuple(a: A): DBTuple2[A, A1, A2]

  /**
    * Empty Findable that lookups up all values of the type
    */

  final def any: Pattern[A] = Pattern2[A, A1, A2](name, None, None)

  /**
    * Get the schema components for objects of type [[A]] in order to generate Schema to store them
    */
  final override def schemaComponents: Vector[SchemaComponent] = any.toSchemaComponent

  /**
    * Convert an [[A]] into a [[Findable]]
    */
  final override def findable(a: A): DBTuple[A] = toTuple(a)


  /**
    * Extract an [[A]] from a DBObject
    */

  final override def fromRow(row: DBObject): ExtractError \/ A =
    if (row.fields.length == 2) {
      for {
        a1 <- s1.get(row.fields(0))
        a2 <- s2.get(row.fields(1))
      } yield construct(a1, a2)
    } else LengthMismatch(row.fields.length, 2).left

  /**
    * Defines how to make a findable with certain fields
    */

  final def pattern(o1: Option[A1], o2: Option[A2]): Pattern2[A, A1, A2] = Pattern2[A, A1, A2](name, o1, o2)
}

abstract class SchemaObject3[A, A1, A2, A3](implicit s1: Storeable[A1], s2: Storeable[A2], s3: Storeable[A3]) extends SchemaObject[A] {
  /**
    * Need to define a constructor for an [[A]] from the components gained from the table
    */

  def construct(a1: A1, a2: A2, a3: A3): A

  /**
    * Need to define the expected name of the table
    */
  def name: TableName

  /**
    * Need to define how to convert an [[A]] into some storeable primitives
    */
  def toTuple(a: A): DBTuple3[A, A1, A2, A3]

  /**
    * Empty Findable that lookups up all values of the type
    */
  final def any: Pattern[A] = Pattern3[A, A1, A2, A3](name, None, None, None)

  /**
    * Get the schema components for objects of type [[A]] in order to generate Schema to store them
    */
  final override def schemaComponents: Vector[SchemaComponent] = any.toSchemaComponent

  /**
    * Convert an [[A]] into a [[Findable]]
    */
  final override def findable(a: A): DBTuple[A] = toTuple(a)

  /**
    * Extract an [[A]] from a DBObject
    */

  final override def fromRow(row: DBObject): ExtractError \/ A =
    if (row.fields.length == 3) {
      for {
        a1 <- s1.get(row.fields(0))
        a2 <- s2.get(row.fields(1))
        a3 <- s3.get(row.fields(2))
      } yield construct(a1, a2, a3)
    } else LengthMismatch(row.fields.length, 3).left

  /**
    * Defines how to make a findable with certain fields
    */

  final def pattern(o1: Option[A1], o2: Option[A2], o3: Option[A3]): Pattern3[A, A1, A2, A3] = Pattern3[A, A1, A2, A3](name, o1, o2, o3)
}


abstract class SchemaObject4[A, A1, A2, A3, A4](implicit s1: Storeable[A1], s2: Storeable[A2], s3: Storeable[A3], s4: Storeable[A4]) extends SchemaObject[A] {
  /**
    * Need to define a constructor for an [[A]] from the components gained from the table
    */
  def construct(a1: A1, a2: A2, a3: A3, a4: A4): A

  /**
    * Need to define the expected name of the table
    */
  def name: TableName

  /**
    * Need to define how to convert an [[A]] into some storeable primitives
    */

  def toTuple(a: A): DBTuple4[A, A1, A2, A3, A4]

  /**
    * Empty Findable that lookups up all values of the type
    */
  final def any: Pattern[A] = Pattern4[A, A1, A2, A3, A4](name, None, None, None, None)

  /**
    * Get the schema components for objects of type [[A]] in order to generate Schema to store them
    */
  final override def schemaComponents: Vector[SchemaComponent] = any.toSchemaComponent

  /**
    * Convert an [[A]] into a [[Findable]]
    */
  final override def findable(a: A): DBTuple[A] = toTuple(a)

  /**
    * Extract an [[A]] from a DBObject
    */


  final override def fromRow(row: DBObject): ExtractError \/ A =
    if (row.fields.length == 4) {
      for {
        a1 <- s1.get(row.fields(0))
        a2 <- s2.get(row.fields(1))
        a3 <- s3.get(row.fields(2))
        a4 <- s4.get(row.fields(3))
      } yield construct(a1, a2, a3, a4)
    } else LengthMismatch(row.fields.length, 4).left

  /**
    * Defines how to make a findable with certain fields
    */

  final def pattern(o1: Option[A1], o2: Option[A2], o3: Option[A3], o4: Option[A4]): Pattern4[A, A1, A2, A3, A4] = Pattern4[A, A1, A2, A3, A4](name, o1, o2, o3, o4)
}