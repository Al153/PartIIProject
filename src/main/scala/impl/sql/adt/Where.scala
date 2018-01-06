package impl.sql.adt

import core.backend.intermediate.unsafe.UnsafeFindable
import impl.sql.jdbc.Conversions
import impl.sql.names.SQLColumnName.{column, leftId, rightId}

sealed trait WhereTable
sealed trait Where
case class Pattern(p: UnsafeFindable) extends WhereTable
case object Distinct extends Where
case object NoConstraint extends Where with WhereTable
case class Limit(limit: VarName, n: Int) extends Where

object Where {
  def render(w: Where): String = w match {
    case NoConstraint => ""
    case Distinct => s"WHERE $leftId != $rightId"
    case Limit(lim, n) => s"WHERE $lim < $n"
  }
}

object WhereTable {
  def render(w: WhereTable): String = w match {
    case Pattern(f) =>
      val conditions = f.pattern.zipWithIndex.collect {
        case (Some(v), i) => s"${column(i)} = ${Conversions.dbCellToSQLValue(v)}"
      }
      if (conditions.nonEmpty) "WHERE " + conditions.mkString(" AND ") else ""

    case NoConstraint => ""
  }
}