package impl.sql.adt

import core.user.dsl.View
import impl.sql._
import impl.sql.adt.CompilationContext.Compilation
import impl.sql.names.{RelationTableName, SQLColumnName}
import impl.sql.tables.ViewsTable._
import queries._

/**
  * Created by Al on 23/11/2017.
  *
  * Methods for getting information about definitions in the SQL tree
  *
  * Input is a compilation, returns a string representing the input having been rendered
  */
object Definitions {

  /**
    * Sets up all definitions around a compiled query
    * @param compilation - compiled query
    * @param view - view to execute on
    * @param outer - the code that extracts results (e.g. fields) from the main query
    * @return - a nicely wrapped SQL Query
    */
  def compute(
               compilation: Compilation[Query],
               view: View
           )(outer: String)(implicit instance: SQLInstance): SQLEither[String] = {
        getDefinitions(compilation, view).map(_+ " (" + outer + ")")
    }

    /**
      *
      */

    def getDefinitions(compilation: Compilation[Query], view: View)(implicit instance: SQLInstance): SQLEither[String] = {
      val (context, mainQuery) = compilation.run(Query.emptyContext)
      for {
        defs <- context.getDefs(instance)
        // get all the definitions of relations etc
        (tableDefs, auxDefs, relationDefs) = defs
        commonSubExpressions = context.commonSubExpressions


        // make one big CTE (WITH ... AS ..., ... AS ... etc
        relations = relationDefs map {
          case (rt, varName) => varName.toString + " AS " + optionalSelect(getRelationWithView(rt.name))
        }

        tables = tableDefs map {
          case (ot, varName) => s"$varName AS " + optionalSelect(ot.name.toString)
        }

        auxTables = auxDefs map {
          case (at, varName) => s"$varName AS (${at.query})"
        }

        subexpressions = commonSubExpressions map {
          case (q, varName) => q match {
            case SimpleSubExpr(q1) =>
              s"${varName.toString} AS " + optionalSelect(Query.render(q1))
            case Recursive(q1) =>
              s"${varName.toString} AS " + optionalSelect(Query.render(q1))
          }
        }

        mainQueryPair = SQLDB.mainQuery + " AS " + optionalSelect(Query.render(mainQuery))
      } yield   (List(view.definition) ++ relations ++ tables ++ auxTables ++ subexpressions ++ List(mainQueryPair)).mkString(", ")
    }

  /**
    * Picks out related pairs that are visible in a view
    */
  private def getRelationWithView(r: RelationTableName): String =
    s"SELECT ${SQLColumnName.leftId}, ${SQLColumnName.rightId} FROM ${r.name} " +
      s"JOIN $viewVar " +
      s"ON ${r.name}.${SQLColumnName.commitId} = $viewVar.${SQLColumnName.commitId}"

}
