package impl.sql.adt

import impl.sql.adt.CompilationContext.Compilation

/**
  * Created by Al on 22/12/2017.
  */
package object queries {
  /**
    * Optionally wrap a string in brackets if it is a subexpression
    */
  def optionalBrackets(s: String): String = if (s.contains(" ")) s"($s)" else s

  /**
    * Wrap a query in an optional Select to ge around postgres synax
    */
  def optionalSelect(s: String): String = if (s.contains(" ")) s"($s)" else s"(SELECT * FROM $s)"

  /**
    * Adds an alias to a subquery if needed, used to get around bizarre PostgreSQL syntax ruless

    */

  def optionalAlias(q: Query): Compilation[Query] = q match {
    case SelectWhere(_, _, _) | SelectTable(_, _) => for {
      alias <- CompilationContext.newSymbol
    } yield Alias(alias, q)
    case Var(_) | Alias(_, _) => CompilationContext.point(q)
    case _ =>  CompilationContext.point(q)
  }
}
