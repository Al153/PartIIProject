package impl.sql.adt

import core.view.View
import impl.sql._
import impl.sql.adt.CompilationContext.Compilation
import impl.sql.tables.ViewsTable._

/**
  * Created by Al on 23/11/2017.
  *
  * Methods for getting information about definitions in the SQL tree
  */
object Definitions {
  def compute(
               compilation: Compilation[Query],
               view: View
           )(outer: String)(implicit instance: SQLInstance): SQLEither[String] = {
      val (context, mainQuery) = compilation.run(Query.emptyContext)
      for {
        defs <- context.getDefs(instance)
        (tableDefs, relationDefs) = defs
        commonSubExpressions = context.commonSubExpressions

        relations = relationDefs map {
          case (rt, varName) => varName.toString + " AS " +
            "(" + getRelationWithView(rt.name, view) + ")"
        }

        tables = tableDefs map {
          case (ot, varName) => s"$varName AS (SELECT * FROM $ot)"
        }

        subexpressions = commonSubExpressions map {
          case (q, varName) => q match {
            case SimpleSubExpr(q1) =>
              s"${varName.toString} AS (${Query.render(q1)})"
            case Recursive(q1) =>
              s"${varName.toString} AS (${Query.render(q1)})"
          }
        }

        mainQueryPair = SQLDB.mainQuery + " AS (" + Query.render(mainQuery) + ")"
      } yield    (List(view.definition) ++ relations ++ tables ++ subexpressions ++ List(mainQueryPair)).mkString(", ") + "(" + outer + ")"
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
