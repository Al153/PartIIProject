package impl.sql

import core.intermediate.unsafe._
import core.schema.TableName
import impl.sql.CompilationContext.Compilation
import impl.sql.compile.VarName
import core.utils._
import SQLDB._
import core.backend.common.DBCell

import scalaz.State

/**
  * A query should expose a left_id and right_id to allow composability
  */
sealed trait Query
case class With(defn:(VarName, Query), in: Query) extends Query
case class WithView(name: VarName, body: Query, in: Query) extends Query // needs to drop view
case class WithRec(name: VarName, body: Query, in: Query) extends Query
case class Var(v: VarName) extends Query
case class SelectWhere(mappings: SelectMapping, where: Where, from: Query) extends Query
case class SelectTable(tableName: VarName, where: WhereTable)
case class IntersectAll(left: Query, right: Query) extends Query
case class UnionAll(left: Query, right: Query) extends Query
case class JoinRename(leftMapping: (VarName, Query), rightMapping: (VarName, Query), on: JoinMapping) extends Query // ($left as $asLeft) join ($right as $asRight) on $on
case class JoinSimple(left: VarName, right: VarName, on: JoinMapping) extends Query // $left join $right on Mapping


sealed trait WhereTable
sealed trait Where
case class Pattern(p: UnsafeFindable) extends WhereTable
case object Distinct extends Where
case object NoConstraint extends Where with WhereTable

sealed trait SelectMapping
case object All extends SelectMapping // Select * from ...
case object Simple extends SelectMapping // Select left_id, right_id from ...
case object FromObject extends SelectMapping // Select (id as left_id, id as right_id) from ...
case class Joined(a: VarName, b: VarName) extends SelectMapping // select a.left_id as left_id, b.right_id as right_id
case class SameSide(a: VarName) extends SelectMapping // select a.left_id as left_id,  a.right_id as right_id
case class ReversedRelation(r: VarName) extends SelectMapping // select r.right_id as left_id, r.left_id as right_id


// can infer variable names from context
sealed trait JoinMapping
case object Chained extends JoinMapping // Join ... on a.right_id = b.left_id
case object OnRight extends JoinMapping // join ... on a.right_id = b.right_id

case class CompletedPairQuery(q: Query, leftTable: SQLTableName, rightTable: SQLTableName) {
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
  }

  private def renderWhere(w: Where): String = w match {
    case NoConstraint => ""
    case Distinct => s"WHERE $leftId != $rightId"
  }

  private def renderWhereTable(w: WhereTable): String = w match {
    case Pattern(p) => "WHERE " + p.pattern.zipWithIndex.collect {
      case (Some(v), i) => s"${column(i)} == ${dbCellTovalue(v)}"
    }.mkString(" AND ")

    case NoConstraint => ""
  }

  def dbCellTovalue(d: DBCell): String = ???

  def render: String = {
    // render query to string
    val baseQuery = renderQuery(q)
    ???
  }
}

case class CompilationContext(
                               varCount: Int,
                               relations: Map[ErasedRelationAttributes, VarName],
                               requiredTables: Map[TableName, VarName] // tables needed for query.
                             ) {

  def newVarName: (CompilationContext, VarName) = (CompilationContext(varCount + 1, relations, requiredTables), VarName("Var" + varCount))

  def getRelationName(r: ErasedRelationAttributes): (CompilationContext, VarName)  =
    if (r in relations) (this, relations(r))
    else {
      val newName = VarName("Relation"  + varCount )
      (CompilationContext(varCount + 1, relations + (r -> newName), requiredTables), newName)
    }

  def getTableName(name: TableName): (CompilationContext, VarName) =
    if (name in requiredTables) (this, requiredTables(name))
    else {
      val newName = VarName("Relation"  + varCount )
      (CompilationContext(varCount + 1, relations, requiredTables + (name -> newName)), newName)
    }

  def getRelationDefs = ??? // idea: get definitions of relations
  def getTableDefs = ??? // ditto but for object tables
}

object CompilationContext {
  type Compilation[A] = State[CompilationContext, A]

  def newVarName: Compilation[VarName] = State {
    initial: CompilationContext =>
      initial.newVarName
  }

  def getRelationName(r: ErasedRelationAttributes): Compilation[VarName] = State {
    initial: CompilationContext =>
      initial.getRelationName(r)
  }

  def getTableName(name: TableName): Compilation[VarName] = State {
    initial: CompilationContext =>
      initial.getTableName(name)
  }

  def point[A](a: => A): Compilation[A] = State(s => (s, a))
}

object Query {
  def emptyContext: CompilationContext = new CompilationContext(0, Map(), Map())

  def fromADT(q: UnsafeFindPair, leftTable: SQLTableName, rightTable: SQLTableName): Compilation[CompletedPairQuery] =
    for {query <- convert(q)} yield CompletedPairQuery(query, leftTable, rightTable)

  def convert(q: UnsafeFindPair): Compilation[Query] = q match  {
    case USAnd(left, right) => for {
      a <- CompilationContext.newVarName
      b <- CompilationContext.newVarName
    } yield IntersectAll(Var(a), Var(b))

    case USAndSingle(left, right) => for {
      l <- convert(left)
      r <- convertSingle(right)
      a <- CompilationContext.newVarName
      b <- CompilationContext.newVarName
    } yield SelectWhere(SameSide(a), NoConstraint, JoinRename(a -> l, b -> r, OnRight))

    case USAtleast(n , rel) => for {
      precomputed <- convert(rel) // precompute a view
      temporaryView <- CompilationContext.newVarName // get a name for it
      rec <- CompilationContext.newVarName // create a name to recurse on
      preTraversal <- getExactly(temporaryView, n) // computer the traversal up to the start of the upto
      preTraversalName <- CompilationContext.newVarName // give it a name
    } yield WithView(
      temporaryView, precomputed, // precompute the repeated relation and put into a view
      With(preTraversalName -> preTraversal, // compute the pretraversal and give it a name
        WithRec(rec, // create a recursive query
          UnionAll(
            SelectWhere(Simple, NoConstraint, Var(preTraversalName)), // basis case: pretraversal
            SelectWhere( // add values to it
              Joined(rec, temporaryView),
              NoConstraint,
              JoinSimple(rec, temporaryView, Chained))
          ),
          SelectWhere(Simple, NoConstraint, Var(rec))
        )
      )
    )

    case USBetween(low, high, rel) => for {
      view <- convert(rel)
      viewName <- CompilationContext.newVarName
      preTraversal <- getExactly(viewName, low)
      preTraversalName <- CompilationContext.newVarName
      postTraversal <- getUpto(viewName, high-low)
      postTraversalName <- CompilationContext.newVarName
    } yield WithView(viewName, view,
        SelectWhere(
          Joined(preTraversalName, postTraversalName),
          NoConstraint,
          JoinRename(
            preTraversalName -> preTraversal,
            postTraversalName -> postTraversal,
            Chained
          )
        )

    )

    case USChain(left, right) => for {
      l <- convert(left)
      r <- convert(right)
      a <- CompilationContext.newVarName
      b <- CompilationContext.newVarName
    } yield SelectWhere(Joined(a, b), NoConstraint, JoinRename(a -> l, b -> r, Chained))

    case USDistinct(rel) =>  for {
      r <- convert(rel)
    } yield SelectWhere(Simple, Distinct, r)

    case USExactly(n, rel) => for {
      view <- convert(rel)
      viewName <- CompilationContext.newVarName
      body <- getExactly(viewName, n)
    } yield WithView(viewName, view, body)

    case USId(tableName) => for {
      n <- CompilationContext.getTableName(tableName)
    } yield SelectWhere(Simple, NoConstraint, Var(n))

    case USNarrow(rel, findable) => for {
      l <- convert(rel)
      r <- doFind(findable)
      a <- CompilationContext.newVarName
      b <- CompilationContext.newVarName
    } yield SelectWhere(SameSide(a), NoConstraint, JoinRename(a -> l, b -> r, OnRight))

    case USOr(left, right) =>  for {
      l <- convert(left)
      r <- convert(right)
    } yield UnionAll(l, r)

    case USRel(rel) => for {
      r <- CompilationContext.getRelationName(rel)
    } yield SelectWhere(All, NoConstraint, Var(r))

    case USRevRel(rel) => for {
      r <- CompilationContext.getRelationName(rel)
    } yield SelectWhere(ReversedRelation(r), NoConstraint, Var(r))

    case USUpto(n, rel) => for {
      view <- convert(rel)
      viewName <- CompilationContext.newVarName
      body <- getUpto(viewName, n)
    } yield WithView(viewName, view, body)
  }

  def convertSingle(q: UnsafeFindSingle): Compilation[Query] = q match {
    case USFind(findable) => doFind(findable)

    case USFrom(start, rel) => for {
      a <- CompilationContext.newVarName
      b <- CompilationContext.newVarName
      start <- convertSingle(start)
      rel <- convert(rel)
    } yield SelectWhere(Joined(a, b), NoConstraint, JoinRename(a -> start, b -> rel, Chained))


    case USNarrowS(start, findable) => for {
      a <- CompilationContext.newVarName
      b <- CompilationContext.newVarName
      start <- convertSingle(start)
      filter <- doFind(findable)
    } yield SelectWhere(SameSide(a), NoConstraint, JoinRename(a -> start, b -> filter, OnRight))

  }

  def getExactly(precomputed: VarName, n: Int): Compilation[Query] = {
    ??? // does an exactly with  precomputed table

    // idea use binary multiplication

    def joinBySquares(n: Int, x: Query): Compilation[Query] =
      if (n <= 0) ???
      else if (n <= 1) CompilationContext.point(x)
      else if (n % 2 == 0)
        for {
          a <- CompilationContext.newVarName
          b <- CompilationContext.newVarName
          doubled <- joinBySquares(
            n/2,

              SelectWhere(
                Joined(a, b),
                NoConstraint,
                JoinRename(
                  a -> x,
                  b -> x,
                  Chained
                )
              )
          )
        } yield doubled
      else
        for {
          a <- CompilationContext.newVarName
          b <- CompilationContext.newVarName
          doubled <- joinBySquares(
            (n-1)/2,


              SelectWhere(
                Joined(a, b),
                NoConstraint,
                JoinRename(
                  a -> x,
                  b -> x,
                  Chained
                )
              )
          )
          l <- CompilationContext.newVarName
          r <- CompilationContext.newVarName
        } yield SelectWhere(Joined(l, r), NoConstraint, JoinRename(l -> x, r -> doubled, Chained))

  }

  def getUpto(precomputed: VarName, n: Int): Compilation[Query] = {
    // do an upto with a precomputed view
    ???
  }

  def doFind(findable: UnsafeFindable): Compilation[Query] = ??? // returns SQL code to do a find of a findable
}