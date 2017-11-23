package impl.sql.adt

import core.backend.common.DBCell
import core.error.E
import core.intermediate.unsafe.{UnsafeFindPair, UnsafeFindable}
import core.schema.SchemaDescription
import core.utils._
import core.view.View
import impl.sql.SQLColumnName.{column, leftId, objId, rightId}
import impl.sql._
import impl.sql.errors.{SQLRelationMissing, SQLTableMissing}
import impl.sql.tables.ViewsTable

import scalaz.\/

case class CompletedPairQuery(
                               p: UnsafeFindPair,
                               leftTable: SQLTableName,
                               rightTable: SQLTableName,
                               sd: SchemaDescription
                             )(implicit instance: SQLInstance) {
  def render(v: View): E \/ String = {
    // render query to string

    val (context, q) = Query.convert(p).run(Query.emptyContext)
    val precomputedView = PrecomputedView() // generate a view to get all the commit ids

    for {
      tableDefs <- EitherOps.sequence(
        for {
          (name, sqlName) <- context.getTableDefs
        } yield instance.tableLookup.getOrError(name, SQLTableMissing(name)).withSnd(sqlName))


      relationDefs <- EitherOps.sequence(for {
        (rel, sqlName) <- context.getRelationDefs
      } yield instance.relationLookup.getOrError(rel, SQLRelationMissing(rel)).withSnd(sqlName))


      baseQuery = renderQuery(q)

      leftPrototype <- sd.lookupTable(p.leftMostTable)
      rightPrototype <- sd.lookupTable(p.rightMostTable)

    } yield ViewsTable.wrapView(v, precomputedView) {
      s"""
         |WITH ${getDefinitions(relationDefs, tableDefs, baseQuery, v, precomputedView)}
         | ${extractMainQuery(leftPrototype.prototype, rightPrototype.prototype, leftTable, rightTable)}
        """.stripMargin
    }
  }


  private def renderQuery(q: Query): String = q match {
    case With((name, body), in) => s"WITH $name AS (${renderQuery(body)}) (${renderQuery(in)})"
    case WithView(name, body, in) => s"CREATE VIEW $name AS (${renderQuery(body)}); ${renderQuery(q)}; DROP VIEW $name"
    case WithRec(name, body, in) => s"WITH RECURSIVE $name AS (${renderQuery(body)}) (${renderQuery(in)})"
    case Var(v) => v.s
    case SelectTable(name, where) => s"SELECT ${renderSelectMapping(FromObject)} FROM $name ${renderWhereTable(where)}"
    case SelectWhere(mappings, where, from) => s"SELECT (${renderSelectMapping(mappings)}) FROM (${renderQuery(from)}) ${renderWhere(where)}"
    case IntersectAll(left, right) => s"(${renderQuery(left)}) INTERSECT (${renderQuery(right)})"
    case UnionAll(left, right) => s"(${renderQuery(left)}) UNION ALL (${renderQuery(right)})"
    case JoinRename((asLeft, left), (asRight, right), on) => s"((${renderQuery(left)}) as $asLeft) INNER JOIN ((${renderQuery(left)}) as $asRight) ON (${renderJoinMapping(on, asLeft, asRight)})"
    case JoinSimple(l, r, on) => s"$l INNER JOIN $r ON (${renderJoinMapping(on, l, r)})"
  }

  private def renderJoinMapping(joinMapping: JoinMapping, a: VarName, b: VarName): String = joinMapping match {
    case Chained => s"$a.$rightId = $b.$leftId"
    case OnRight =>s"$a.$rightId = $b.$rightId"
  }

  private def renderSelectMapping(s: SelectMapping): String = s match {
    case All => "*"
    case Simple => s"$leftId, $rightId"
    case FromObject => s"$objId as $leftId, $objId as $rightId"
    case Joined(a, b) => s"$a.$leftId as $leftId, $b.$rightId as $rightId"
    case SameSide(a) => s"$a.$leftId as $leftId, $a.$rightId as $rightId"
    case ReversedRelation(a) => s"$a.$rightId as $leftId, $a.$leftId as $rightId"
    case WithLimit(lim, rest) => s"${renderSelectMapping(rest)}, $lim + 1 as $lim"
    case StartLimit(lim, rest) => s"${renderSelectMapping(rest)}, 0 as $lim"
  }

  private def renderWhere(w: Where): String = w match {
    case NoConstraint => ""
    case Distinct => s"WHERE $leftId != $rightId"
    case Limit(lim, n) => s"WHERE $lim < $n"
  }

  private def renderWhereTable(w: WhereTable): String = w match {
    case Pattern(f) => "WHERE " + f.pattern.zipWithIndex.collect {
      case (Some(v), i) => s"${column(i)} == ${dbCellTovalue(v)}"
    }.mkString(" AND ")

    case NoConstraint => ""
  }

  private def dbCellTovalue(d: DBCell): String = d.toString

  // query terms appended to the left and right hand sides of the main query
  // actually pull out values

  private def extractMainQuery(
                        leftProto: UnsafeFindable,
                        rightProto: UnsafeFindable,
                        leftTableName: SQLTableName,
                        rightTableName: SQLTableName
                      ): String = {
    s"SELECT ${getColumns(leftProto, rightProto)} " +
      s"FROM ${getRightJoin(rightTableName, getLeftJoin(leftTableName))}"
  }

  private def getDefinitions(
                      relationDefs: Iterable[(RelationTableName, VarName)],
                      tableDefs: Iterable[(ObjectTableName, VarName)],
                      mainQuery: String,
                      v: View,
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

  // we need the findable/width of the query
  // todo:Typesafety
  private def getLeftJoin(tableName: SQLTableName): String =
    s"${getLeftTable(tableName)} JOIN ${SQLDB.mainQuery} " +
      s"ON ${SQLDB.leftmostTable}.${SQLColumnName.objId} = ${SQLDB.mainQuery}.${SQLColumnName.leftId}"

  private def getRightJoin(tableName: SQLTableName, leftAndMainQuery: String): String =
    s"($leftAndMainQuery) JOIN ${getRightTable(tableName)}" +
      s"ON ${SQLColumnName.rightId} = ${SQLDB.rightmostTable}.${SQLColumnName.objId}"

  private def getLeftTable(tableName: SQLTableName): String = s"$tableName as ${SQLDB.leftmostTable}"
  private def getRightTable(tableName: SQLTableName): String = s"$tableName as ${SQLDB.rightmostTable}"

  // construct a query to rename columns
  private def getColumns(
                         leftDescription: UnsafeFindable,
                         rightDescription: UnsafeFindable
                       ): String = {
    val leftPairs =
      (1 until leftDescription.pattern.length).map(i => s"${SQLDB.leftmostTable}.${SQLColumnName.column(i)} as ${SQLColumnName.leftColumn(i)}")
    val rightPairs =
      (1 until rightDescription.pattern.length).map(i => s"${SQLDB.rightmostTable}.${SQLColumnName.column(i)} as ${SQLColumnName.rightColumn(i)}")

    (leftPairs ++ rightPairs).mkString(", ")
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
