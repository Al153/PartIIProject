package impl.sql.adt

import core.backend.intermediate.unsafe.ErasedFindable
import impl.sql.jdbc.Conversions
import impl.sql.names.SQLColumnName.{column, leftId, rightId}

/**
  * Pair of sealed trait hierarchies for the "WHERE ... " fragment of queries
  */
/**
  * Where, when picking values from a table (e.g. we may want constraints on column values)
  */
sealed trait WhereTable

/**
  * Where in general queries
  */
sealed trait Where

/**
  * Where column values match a pattern
  */
case class Pattern(p: ErasedFindable) extends WhereTable

/**
  * Where left_id =/= right_id
  */
case object Distinct extends Where

/**
  * Identity-where
  */
case object NoConstraint extends Where with WhereTable

/**
  * Where an alias is less than a limit
  */
case class Limit(limit: VarName, n: Int) extends Where

/**
  * Where an alias is not equal to zero
  */

case class LimitZero(limit: VarName) extends Where

object Where {
  /**
    * Render by case matching
    */
  def render(w: Where): String = w match {
    case NoConstraint => ""
    case Distinct => s"WHERE $leftId != $rightId"
    case Limit(lim, n) => s"WHERE $lim < $n"
    case LimitZero(lim) => s"WHERE $lim > 0"
  }
}

object WhereTable {
  /**
    * Render by case matching
    */
  def render(w: WhereTable): String = w match {
    case Pattern(f) =>
      val conditions = f.pattern.zipWithIndex.collect {
        case (Some(v), i) => s"${column(i)} = ${Conversions.dbCellToSQLValue(v)}"
      }
      if (conditions.nonEmpty) "WHERE " + conditions.mkString(" AND ") else ""

    case NoConstraint => ""
  }
}