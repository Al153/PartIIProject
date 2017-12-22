package impl.sql.adt

import impl.sql._
import impl.sql.tables.{ObjectTable, RelationTable}

/**
  * Created by Al on 23/11/2017.
  *
  * Methods for getting information about definitions in the SQL tree
  */
object Definitions {
  def get(
           relationDefs: Iterable[(RelationTable, VarName)],
           tableDefs: Iterable[(ObjectTable, VarName)],
           commonSubExpressions: Iterable[(Query, VarName)],
           mainQuery: Query,
           precomputedView: PrecomputedView
         ): String = {
    val relations = relationDefs map {
      case (rt, varName) => varName.toString + " AS " +
        "(" + getRelationWithView(rt.name, precomputedView) + ")"
    }

    val tables = tableDefs map {
      case (ot, varName) => varName.toString + " AS " +
        "(" + getTableWithView(ot.name, precomputedView) + ")"
    }

    val subexpressions = commonSubExpressions map {
      case (q, varName) =>
        varName.toString + " AS " +
          "(" + Query.render(q) + ")"
    }

    val mainQueryPair = SQLDB.mainQuery + " AS (" + Query.render(mainQuery) + ")"
    (relations ++ tables ++ subexpressions ++ List(mainQueryPair)).mkString(", ")
  }

  // selects with matching view values
  private def getRelationWithView(r: RelationTableName, precomputedView: PrecomputedView):String =
    s"SELECT ${SQLColumnName.leftId}, ${SQLColumnName.rightId} FROM ${r.name} " +
      s"JOIN $precomputedView " +
      s"ON ${r.name}.${SQLColumnName.commitId} = $precomputedView.${SQLColumnName.commitId}"

  def getTableWithView(r: ObjectTableName, precomputedView: PrecomputedView): String =
    s"SELECT ${SQLColumnName.objId} FROM ${r.name} " +
      s"JOIN $precomputedView " +
      s"ON ${r.name}.${SQLColumnName.commitId} = $precomputedView.${SQLColumnName.commitId}"



}
