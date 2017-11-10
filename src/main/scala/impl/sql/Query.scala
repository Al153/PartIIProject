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
case class SelectWhere(mappings: SelectMapping, where: Where, from: Query) extends Query
case class selectTable(mappings: SelectMapping, from: SQLTableName) extends Query
case class IntersectAll(left: Query, right: Query) extends Query
case class UnionAll(left: Query, right: Query) extends Query
case class IntersectRight(left: Query, right: Query) extends Query // pick entries in result of left which match
case class Join(a: Query, right: Query, on: JoinMapping) extends Query

sealed trait selectable
sealed trait Where
case class Pattern(p: UnsafeFindable) extends Where
case object Distinct extends Where

sealed trait SelectMapping
case object All extends SelectMapping // Select * from ..
case object Simple extends SelectMapping // Select left_id, right_id from
case class Joined(a: VarName, b: VarName) extends SelectMapping // select a.left_id as left_id, b.right_id as right_id
case class SameSide(a: VarName) extends SelectMapping // select a.left_id as left_id,  a.right_id as right_id
case class Reversed(a: VarName, b: VarName) extends SelectMapping // select b.left_id as left_id, a.right_id as right_id

sealed trait JoinMapping
case class Chained(a: VarName, b: VarName) extends SelectMapping // Join ... on a.right_id = b.left_id
// todo: needed?
case class ReverseChained(a: VarName, B: VarName) extends SelectMapping // join ... on a.left_id = b.right_id