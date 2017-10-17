package schema

/**
  * Created by Al on 17/10/2017.
  */
sealed trait SchemaObject[A] {
  def getSchemaSummary: List[SchemaSummary]
  def generalPattern: Pattern[A]
  def findable(a: A): Findable[A]
}

trait SchemaObject0[A] extends SchemaObject[A] {
  def construct(): A
  def name: TableName
  def toTuple(a: A): DBTuple0[A]

  final def fromTuple(tuple0: DBTuple0[A]): A = construct()
  final def pattern: Pattern0[A] = Pattern0[A]()
  final def generalPattern: Pattern[A] = pattern
  final override def getSchemaSummary: List[SchemaSummary] = pattern.toSchemaSummary
  final override def findable(a: A): Findable[A] = toTuple(a)
}

abstract class SchemaObject1[A, A1](implicit s1: Storeable[A1]) extends SchemaObject[A] {
  def construct(a1: A1): A
  def name: TableName
  def toTuple(a: A): DBTuple1[A, A1]

  final def fromTuple(tuple1: DBTuple1[A, A1]): A = construct(tuple1.a1)
  final def generalPattern: Pattern[A] = pattern
  final def pattern: Pattern1[A, A1] = Pattern1[A, A1](None)
  final override def getSchemaSummary: List[SchemaSummary] = pattern.toSchemaSummary
  final override def findable(a: A): Findable[A] = toTuple(a)
}

abstract class SchemaObject2[A, A1, A2](implicit s1: Storeable[A1], s2: Storeable[A2]) extends SchemaObject[A] {
  def construct(a1: A1, a2: A2): A
  def name: TableName
  def toTuple(a: A): DBTuple2[A, A1, A2]

  final def fromTuple(tuple2: DBTuple2[A, A1, A2]): A = construct(tuple2.a1, tuple2.a2)
  final def generalPattern: Pattern[A] = pattern
  final def pattern: Pattern2[A, A1, A2] = Pattern2[A, A1, A2](None, None)
  final override def getSchemaSummary: List[SchemaSummary] = pattern.toSchemaSummary
  final override def findable(a: A): Findable[A] = toTuple(a)
}

abstract class SchemaObject3[A, A1, A2, A3](implicit s1: Storeable[A1], s2: Storeable[A2], s3: Storeable[A3]) extends SchemaObject[A] {
  def construct(a1: A1, a2: A2, a3: A3): A
  def name: TableName
  def toTuple(a: A): DBTuple3[A, A1, A2, A3]

  final def fromTuple(tuple3: DBTuple3[A, A1, A2, A3]): A = construct(tuple3.a1, tuple3.a2, tuple3.a3)
  final def generalPattern: Pattern[A] = pattern
  final def pattern: Pattern3[A, A1, A2, A3] = Pattern3[A, A1, A2, A3](None, None, None)
  final override def getSchemaSummary: List[SchemaSummary] = pattern.toSchemaSummary
  final override def findable(a: A): Findable[A] = toTuple(a)
}