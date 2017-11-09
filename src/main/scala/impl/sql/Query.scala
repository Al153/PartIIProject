package impl.sql

import core.intermediate.unsafe.UnsafeFindable
import impl.sql.compile.VarName

/**
  * A query should expose a left_id and right_id to allow composability
  */
sealed trait Query
case class With(name: VarName, body: Query, in: Query) extends Query
case class WithRec(name: VarName, body: Query, in: Query) extends Query
case class Var(v: VarName) extends Query
case class SelectWhere(where: UnsafeFindable, from: Query) extends Query
case class IntersectAll(left: Query, right: Query) extends Query
