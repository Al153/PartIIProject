package impl.sql

import core.intermediate.unsafe.UnsafeFindable
import impl.sql.compile.VarName

/**
  * A query should expose a left_id and right_id to allow composability
  */
sealed trait Query
case class With(defs: Seq[(VarName, Query)], in: Query) extends Query
case class WithRec(name: VarName, body: Query, in: Query) extends Query
case class Var(v: VarName) extends Query
case class SelectWhere(where: Where, from: Query) extends Query
case class selectTable(from: SQLTableName) extends Query
case class IntersectAll(left: Query, right: Query) extends Query
case class UnionAll(left: Query, right: Query) extends Query
case class IntersectRight(left: Query, right: Query) extends Query // pick entries in result of left which match

sealed trait selectable
sealed trait Where
case class Pattern(p: UnsafeFindable) extends Where
case object Distinct extends Where