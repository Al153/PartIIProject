package impl.sql.adt

import impl.sql._

/**
  * Created by Al on 23/11/2017.
  *
  * Methods for getting information about definitions in the SQL tree
  */
object Definitions {
  def get(
           relationDefs: Iterable[(RelationTableName, VarName)],
           tableDefs: Iterable[(ObjectTableName, VarName)],
           mainQuery: String,
           precomputedView: SQLTableName
         ): String = {
    val relations = relationDefs map {
      case (rtName, varName) => varName.toString + " AS " +
        "(" + getRelationWithView(rtName, precomputedView) + ")"
    }

    val tables = tableDefs map {
      case (otName, varName) => varName.toString + " AS " +
        "(" + getTableWithView(otName,precomputedView) + ")"
    }

    val mainQueryPair = SQLDB.mainQuery + " as (" + mainQuery + ")"
    (relations ++ tables ++ List(mainQueryPair)).mkString(", ")
  }

  // selects with matching view values
  private def getRelationWithView(r: RelationTableName, precomputedView: SQLTableName):String =
    s"SELECT ${SQLColumnName.leftId}, ${SQLColumnName.rightId} FROM ${r.name} " +
      s"JOIN $precomputedView" +
      s"ON ${r.name}.${SQLColumnName.commitId} = $precomputedView.${SQLColumnName.commitId}"

  private def getTableWithView(r: ObjectTableName, precomputedView: SQLTableName): String =
    s"SELECT ${SQLColumnName.objId} FROM ${r.name} " +
      s"JOIN $precomputedView " +
      s"ON ${r.name}.${SQLColumnName.commitId} = $precomputedView.${SQLColumnName.commitId}"

}
