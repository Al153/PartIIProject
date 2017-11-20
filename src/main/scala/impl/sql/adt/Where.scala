package impl.sql.adt

import core.intermediate.unsafe.UnsafeFindable

sealed trait WhereTable
sealed trait Where
case class Pattern(p: UnsafeFindable) extends WhereTable
case object Distinct extends Where
case object NoConstraint extends Where with WhereTable
case class Limit(limit: VarName, n: Int) extends Where