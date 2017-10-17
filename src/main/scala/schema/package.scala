import core.NodeDef
import schema.typeclasses.DatabaseValue

/**
  * Created by Al on 01/10/2017.
  *
  * Part of creating the DSL.
  * A SchemaObject maps to the schema representation of an object in the table
  *
  * A set of schema should have the user's defined case classes and a set of implicit conversions to and from the appropriate SchemaObject
  */

package object schema {
  type C = CellEntry
  type Evidence[T] = T => CellEntry
  
  sealed trait SchemaObject[Res] {
    def unmarshal: Res
  }

  case class SchemaObject0 [Res](name: SchemaName, construct: () => Res) extends SchemaObject[Res] {lazy val unmarshal: Res = construct()}
  case class SchemaObject1 [T1, Res](name: SchemaName, a1: T1, construct: T1 => Res)(implicit e1: Evidence[T1]) extends SchemaObject[Res] {lazy val unmarshal: Res = construct(a1)}
  case class SchemaObject2 [T1, T2, Res](name: SchemaName, a1: T1, a2: T2, construct: (T1, T2) => Res) extends SchemaObject[Res] {lazy val unmarshal: Res = construct(a1, a2)}
  case class SchemaObject3 [T1, T2, T3, Res](name: SchemaName, a1: T1, a2: T2, a3: T3, construct: (T1, T2, T3) => Res) extends SchemaObject[Res] {lazy val unmarshal: Res = construct(a1, a2, a3)}
  case class SchemaObject4 [T1, T2, T3, T4, Res](name: SchemaName,a1: T1, a2: T2, a3: T3, a4: T4, construct: (T1, T2, T3, T4) => Res) extends SchemaObject[Res] {lazy val unmarshal: Res = construct(a1, a2, a3, a4)}
  case class SchemaObject5 [T1, T2, T3, T4, T5, Res](name: SchemaName,a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, construct: (T1, T2, T3, T4, T5) => Res) extends SchemaObject[Res] {lazy val unmarshal: Res = construct(a1, a2, a3, a4, a5)}
  case class SchemaObject6 [T1, T2, T3, T4, T5, T6, Res](name: SchemaName,a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, construct: (T1, T2, T3, T4, T5, T6) => Res) extends SchemaObject[Res] {lazy val unmarshal: Res = construct(a1, a2, a3, a4, a5, a6)}
  case class SchemaObject7 [T1, T2, T3, T4, T5, T6, T7, Res](name: SchemaName,a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, construct: (T1, T2, T3, T4, T5, T6, T7) => Res) extends SchemaObject[Res] {lazy val unmarshal: Res = construct(a1, a2, a3, a4, a5, a6, a7)}
  case class SchemaObject8 [T1, T2, T3, T4, T5, T6, T7, T8, Res](name: SchemaName,a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, construct: (T1, T2, T3, T4, T5, T6, T7, T8) => Res) extends SchemaObject[Res] {lazy val unmarshal: Res = construct(a1, a2, a3, a4, a5, a6, a7, a8)}
  case class SchemaObject9 [T1, T2, T3, T4, T5, T6, T7, T8, T9, Res](name: SchemaName,a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, construct: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => Res) extends SchemaObject[Res] {lazy val unmarshal: Res = construct(a1, a2, a3, a4, a5, a6, a7, a8, a9)}
  case class SchemaObject10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, Res](name: SchemaName,a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, construct: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Res) extends SchemaObject[Res] {lazy val unmarshal: Res = construct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)}

  sealed trait CellEntry {}
  implicit case class CellReference[R](o: SchemaObject[R]) extends CellEntry
  case class CellPrimitive[T](t: T)(implicit p: DatabaseValue[T]) extends CellEntry {
    def getEvidence: DatabaseValue[T] = p
  }


  case class SchemaName(n: String) extends AnyVal {
    def name: String = n
  }


  case class TableHeader(n: String) extends AnyVal {}
  type DbMap = Map[TableHeader, Option[CellEntry]]


}
