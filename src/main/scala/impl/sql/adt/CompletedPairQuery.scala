package impl.sql.adt

import core.backend.common.DBCell
import core.error.E
import core.intermediate.unsafe.{ErasedRelationAttributes, UnsafeFindPair, UnsafeFindable}
import core.utils._
import core.view.View
import impl.sql.SQLDB.{column, leftId, objId, rightId}
import impl.sql.errors.{SQLRelationMissing, SQLTableMissing}
import impl.sql._

import scalaz.\/

case class CompletedPairQuery(p: UnsafeFindPair, leftTable: SQLTableName, rightTable: SQLTableName)(implicit instance: SQLInstance) {
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

  def dbCellTovalue(d: DBCell): String = d.toString

  def render(v: View): E \/ String = {
    // render query to string

    val (context, q) = Query.convert(p).run(Query.emptyContext)

    for {
      tableDefs <- EitherOps.sequence(
        for {
          (name, sqlName) <- context.getTableDefs
        } yield instance.tableLookup.getOrError(name, SQLTableMissing(name)).withSnd(sqlName))


      relationDefs <- EitherOps.sequence(for {
        (rel, sqlName) <- context.getRelationDefs
      } yield instance.relationLookup.getOrError(rel, SQLRelationMissing(rel)).withSnd(sqlName))


      baseQuery = renderQuery(q)
    } yield
      s"""
         |WITH ${getDefinitions(relationDefs, tableDefs, baseQuery, v)}
         | ${extractMainQuery(???, ???, leftTable, rightTable)}
       """.stripMargin
    }



  // query terms appended to the left and right hand sides of the main query
  // actually pull out values

  def extractMainQuery(
                        leftProto: UnsafeFindable,
                        rightProto: UnsafeFindable,
                        leftTableName: SQLTableName,
                        rightTableName: SQLTableName
                      ): String = {
    s"SELECT ${getColumns(leftProto, rightProto)} FROM ${getRightJoin(rightTableName, getLeftJoin(leftTableName))}"
  }

  def getDefinitions(
                      relationDefs: Iterable[(RelationTableName, VarName)],
                      tableDefs: Iterable[(ObjectTableName, VarName)],
                      mainQuery: String,
                      v: View
                    ): String = {
    val relations = relationDefs map {
      case (rtName, varName) => varName.toString + " as (" + getRelationWithView(v, rtName) + ")"
    }

    val tables = tableDefs map {
      case (otName, varName) => varName.toString + " as (" + getTableWithView(v, otName) + ")"
    }

    val mainQueryPair = SQLDB.mainQuery + " as (" + mainQuery + ")"
    (relations ++ tables ++ List(mainQueryPair)).mkString(", ")
  }

  // we need the findable/width of the query
  // todo:Typesafety
  private def getLeftJoin(tableName: SQLTableName): String =
  s"${getLeftTable(tableName)} JOIN ${SQLDB.mainQuery} " +
    s"ON ${SQLDB.leftmostTable}.${SQLDB.objId} = ${SQLDB.mainQuery}.${SQLDB.leftId}"

  private def getRightJoin(tableName: SQLTableName, leftAndMainQuery: String): String =
    s"($leftAndMainQuery) JOIN ${getRightTable(tableName)} ON ${SQLDB.rightId} = ${SQLDB.rightmostTable}.${SQLDB.objId}"

  private def getLeftTable(tableName: SQLTableName): String = s"$tableName as ${SQLDB.leftmostTable}"
  private def getRightTable(tableName: SQLTableName): String = s"$tableName as ${SQLDB.rightmostTable}"

  // construct a query to rename columns
  private def getColumns(
                         leftDescription: UnsafeFindable,
                         rightDescription: UnsafeFindable
                       ): String = {
    val leftPairs =
      (1 until leftDescription.pattern.length).map(i => s"${SQLDB.leftmostTable}.${SQLDB.column(i)} as ${SQLDB.leftColumn(i)}")
    val rightPairs =
      (1 until rightDescription.pattern.length).map(i => s"${SQLDB.rightmostTable}.${SQLDB.column(i)} as ${SQLDB.rightColumn(i)}")

    (leftPairs ++ rightPairs).mkString(", ")
  }

  private def bindMainQuery(query: String): String = s"WITH ${SQLDB.mainQuery} as ($query)"

  // selects with matching view values
  private def getRelationWithView(view: View, r: RelationTableName):String =
    s"SELECT ${SQLDB.leftId}, ${SQLDB.rightId} FROM ${r.name} WHERE ${SQLDB.viewId} = ${view.id}"

  private def getTableWithView(view: View, r: ObjectTableName): String =
    s"SELECT ${SQLDB.objId} FROM ${r.name} WHERE ${SQLDB.viewId} = ${view.id}"
}
