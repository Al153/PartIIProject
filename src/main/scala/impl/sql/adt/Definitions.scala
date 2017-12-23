package impl.sql.adt

import core.view.View
import impl.sql._
import impl.sql.tables.ViewsTable._
import impl.sql.tables.{ObjectTable, RelationTable}

/**
  * Created by Al on 23/11/2017.
  *
  * Methods for getting information about definitions in the SQL tree
  */
object Definitions {
  def withs(
           relationDefs: Iterable[(RelationTable, VarName)],
           tableDefs: Iterable[(ObjectTable, VarName)],
           commonSubExpressions: Iterable[(SubExpression, VarName)],
           view: View,
           mainQuery: Query
         )(outer: String): String = {
    val relations = relationDefs map {
      case (rt, varName) => varName.toString + " AS " +
        "(" + getRelationWithView(rt.name, view) + ")"
    }

    val tables = tableDefs map {
      case (ot, varName) => s"$varName AS (SELECT * FROM $ot)"
    }


    val subexpressions = commonSubExpressions map {
      case (q, varName) => q match {
        case SimpleSubExpr(q1) =>
          varName.toString + " AS " +
          "(" + Query.render(q1) + ")"
        case Recursive(q1) =>
          /* "RECRUSIVE " + */ varName.toString + " AS " +
            "(" + Query.render(q1) + ")"
      }

    }

    val mainQueryPair = SQLDB.mainQuery + " AS (" + Query.render(mainQuery) + ")"
    (List(view.definition) ++ relations ++ tables ++ subexpressions ++ List(mainQueryPair)).mkString(", ") + "(" + outer + ")"
  }

  // selects with matching view values
  private def getRelationWithView(r: RelationTableName, view: View):String =
    s"SELECT ${SQLColumnName.leftId}, ${SQLColumnName.rightId} FROM ${r.name} " +
      s"JOIN ${view.name} " +
      s"ON ${r.name}.${SQLColumnName.commitId} = ${view.name}.${SQLColumnName.commitId}"

  def getTableWithView(r: ObjectTableName, view: View): String =
    s"SELECT ${SQLColumnName.objId} FROM ${r.name} " +
      s"JOIN ${view.name} " +
      s"ON ${r.name}.${SQLColumnName.commitId} = ${view.name}.${SQLColumnName.commitId}"
}
