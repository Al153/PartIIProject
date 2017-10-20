import view.Commit

/**
  * Created by Al on 17/10/2017.
  *
  * testing out conversion from object to schema value
  */
package object schema {



  sealed trait Findable[A] // Can be found in the database (not necessarily complete data)
  sealed trait Writeable[A] // can be written to the database (necessarily complete data)

  sealed trait DBTuple[A] extends Findable[A] with Writeable[A]{
    def toSchemaSummary: Map[ColumnName, SchemaSummary]
  }

  case class DBTuple0[Res]() extends DBTuple[Res] {
    override def toSchemaSummary: Map[ColumnName, SchemaSummary] = Map()
  }

  case class DBTuple1[Res, A1](a1: A1)(implicit s1: Storeable[A1]) extends DBTuple[Res]{
    override def toSchemaSummary: Map[ColumnName, SchemaSummary] = Map(ColumnName("1") -> s1.schemaSummary)
  }

  case class DBTuple2[Res, A1, A2](a1: A1, a2: A2)(implicit s1: Storeable[A1], s2: Storeable[A2]) extends DBTuple[Res]{
    override def toSchemaSummary: Map[ColumnName, SchemaSummary] = Map(ColumnName("1") -> s1.schemaSummary, ColumnName("2") -> s2.schemaSummary)
  }

  case class DBTuple3[Res, A1, A2, A3](a1: A1, a2: A2, a3: A3)(implicit s1: Storeable[A1], s2: Storeable[A2], s3: Storeable[A3]) extends DBTuple[Res]{
    override def toSchemaSummary: Map[ColumnName, SchemaSummary] = Map(ColumnName("1") -> s1.schemaSummary)
  }

  sealed trait Pattern[A] extends Findable[A] {
    def toSchemaSummary: List[SchemaSummary]
  }

  case class Pattern0[Res]() extends Pattern[Res] {
    override def toSchemaSummary: List[SchemaSummary] = List()
    def apply(): Pattern0[Res] = this
  }

  case class Pattern1[Res, A1](a1: Option[A1])(implicit s1: Storeable[A1]) extends Pattern[Res] {
    override def toSchemaSummary: List[SchemaSummary] = List(s1.schemaSummary)
    def apply(a: A1): Pattern1[Res, A1] = Pattern1(Some(a))
  }

  case class Pattern2[Res, A1, A2](a1: Option[A1], a2: Option[A2])(implicit s1: Storeable[A1], s2: Storeable[A2]) extends Pattern[Res] {
    override def toSchemaSummary: List[SchemaSummary] = List(s1.schemaSummary, s2.schemaSummary)
    def apply(a1: Option[A1], a2: Option[A2]): Pattern2[Res, A1, A2] = Pattern2(a1, a2)
  }

  case class Pattern3[Res, A1, A2, A3](a1: Option[A1], a2: Option[A2], a3: Option[A3])(implicit s1: Storeable[A1], s2: Storeable[A2], s3: Storeable[A3]) extends Pattern[Res] {
    override def toSchemaSummary: List[SchemaSummary] = List(s1.schemaSummary, s2.schemaSummary, s3.schemaSummary)
    def apply(a1: Option[A1], a2: Option[A2], a3: Option[A3]): Pattern3[Res, A1, A2, A3] = Pattern3(a1, a2, a3)
  }




  def ?[A](o: SchemaObject0[A]): Pattern0[A] = o.pattern
  def ?[A, A1](o: SchemaObject1[A, A1]): Pattern1[A, A1] = o.pattern
  def ?[A, A1, A2](o: SchemaObject2[A, A1, A2]): Pattern2[A, A1, A2] = o.pattern
  def ?[A, A1, A2, A3](o: SchemaObject3[A, A1, A2, A3]): Pattern3[A, A1, A2, A3] = o.pattern

  def ??[A](o: SchemaObject[A]): Pattern[A] = o.generalPattern

  def !![A](a: A)(implicit schemaObject: SchemaObject[A]): Findable[A] = schemaObject.findable(a)
}

