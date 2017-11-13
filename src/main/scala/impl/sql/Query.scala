package impl.sql

import core.intermediate.unsafe._
import core.schema.TableName
import impl.sql.CompilationContext.Compilation
import impl.sql.compile.VarName
import core.utils._

import scalaz.State

/**
  * A query should expose a left_id and right_id to allow composability
  */
sealed trait Query
case class With(defs: Seq[(VarName, Query)], in: Query) extends Query
case class WithView(name: VarName, body: Query, in: Query) extends Query // needs to drop view
case class WithRec(name: VarName, body: Query, in: Query) extends Query
case class Var(v: VarName) extends Query
case class SelectWhere(mappings: SelectMapping, where: Where, from: Query) extends Query
case class selectTable(mappings: SelectMapping, from: SQLTableName) extends Query
case class IntersectAll(left: Query, right: Query) extends Query
case class UnionAll(left: Query, right: Query) extends Query
case class IntersectRight(left: Query, right: Query) extends Query // pick entries in result of left which match
case class Join(a: Query, right: Query, on: JoinMapping) extends Query


sealed trait Where
case class Pattern(p: UnsafeFindable) extends Where
case object Distinct extends Where
case object NoConstraint extends Where

sealed trait SelectMapping
case object All extends SelectMapping // Select * from ...
case object Simple extends SelectMapping // Select left_id, right_id from ...
case object Duplicated extends SelectMapping // Select (id as left_id, id as right_id) from ...
case class Joined(a: VarName, b: VarName) extends SelectMapping // select a.left_id as left_id, b.right_id as right_id
case class SameSide(a: VarName) extends SelectMapping // select a.left_id as left_id,  a.right_id as right_id
case class Reversed(a: VarName, b: VarName) extends SelectMapping // select b.left_id as left_id, a.right_id as right_id

sealed trait JoinMapping
case class Chained(a: VarName, b: VarName) extends JoinMapping // Join ... on a.right_id = b.left_id
// todo: needed?
case class ReverseChained(a: VarName, B: VarName) extends JoinMapping // join ... on a.left_id = b.right_id

case class CompletedPairQuery(q: Query, leftTable: SQLTableName, rightTable: SQLTableName) {
  def render: String = ??? // render query to string
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
  def getTableDefs = ??? // ditto but for object tables // not needed I thinkhgc
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
}

object Query {
  def emptyContext: CompilationContext = ???

  def fromADT(q: UnsafeFindPair, leftTable: SQLTableName, rightTable: SQLTableName): Compilation[CompletedPairQuery] =
    for {query <- convert(q)} yield CompletedPairQuery(query, leftTable, rightTable)

  def convert(q: UnsafeFindPair): Compilation[Query] = q match  {
    case USAnd(left, right) => for {
      l <- convert(left)
      r <- convert(right)
      a <- CompilationContext.newVarName
      b <- CompilationContext.newVarName
    } yield With(List(a -> l, b -> r), IntersectAll(Var(a), Var(b)))

    case USAndSingle(left, right) => for {
      l <- convert(left)
      r <- convertSingle(right)
      a <- CompilationContext.newVarName
      b <- CompilationContext.newVarName
    } yield With(List(a -> l, b -> r), IntersectRight(Var(a), Var(b)))

    case USAtleast(n , rel) => for {
      precomputed <- convert(rel) // precompute a view
      temporaryView <- CompilationContext.newVarName // get a name for it
      rec <- CompilationContext.newVarName // create a name to recurse on
      preTraversal <- getExactly(temporaryView, n) // computer the traversal up to the start of the upto
      preTraversalName <- CompilationContext.newVarName // give it a name
    } yield WithView(
      temporaryView, precomputed, // precompute the repeated relation and put into a view
      With(List(preTraversalName -> preTraversal), // compute the pretraversal and give it a name
        WithRec(rec, // create a recursive query
          UnionAll(
            SelectWhere(Simple, NoConstraint, Var(preTraversalName)), // basis case: pretraversal
            SelectWhere( // add values to it
              Joined(rec, temporaryView),
              NoConstraint,
              Join(Var(rec), Var(temporaryView), Chained(rec, temporaryView)))
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
      With(
        List(preTraversalName -> preTraversal, postTraversalName -> postTraversal),
        SelectWhere(
          Joined(preTraversalName, postTraversalName),
          NoConstraint,
          Join(
            Var(preTraversalName),
            Var(postTraversalName),
            Chained(preTraversalName, postTraversalName)
          )
        )
      )
    )

    case USChain(left, right) => for {
      l <- convert(left)
      r <- convert(right)
      a <- CompilationContext.newVarName
      b <- CompilationContext.newVarName
    } yield With(List(a -> l, b -> r), SelectWhere(Joined(a, b), NoConstraint, Join(Var(a), Var(b), Chained(a, b))))

    case USDistinct(rel) =>  for {
      r <- convert(rel)
      a <- CompilationContext.newVarName
    } yield With(List(a -> r), SelectWhere(Simple, Distinct, r))

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
    } yield With(List(a -> l, b -> r), IntersectRight(Var(a), Var(b)))

    case USOr(left, right) =>  for {
      l <- convert(left)
      r <- convert(right)
      a <- CompilationContext.newVarName
      b <- CompilationContext.newVarName
    } yield With(List(a -> l, b -> r), UnionAll(Var(a), Var(b)))
      
    case USRel(rel) => for {
      r <- CompilationContext.getRelationName(rel)
    } yield SelectWhere(All, NoConstraint, Var(r))

    case USRevRel(rel) => for {
      r <- CompilationContext.getRelationName(rel)
    } yield SelectWhere(Reversed(r, r), NoConstraint, Var(r))

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
    } yield With(List(a -> start, b -> rel), SelectWhere(Joined(a, b), NoConstraint, Join(Var(a), Var(b), Chained(a, b))))


    case USNarrowS(start, findable) => for {
      a <- CompilationContext.newVarName
      b <- CompilationContext.newVarName
      start <- convertSingle(start)
      filter <- doFind(findable)
    } yield With(List(a -> start, b -> filter), IntersectRight(Var(a), Var(b)))

  }

  def getExactly(precomputed: VarName, n: Int): Compilation[Query] = {
    ??? // does an exactly with  precomputed table
  }

  def getUpto(precomputed: VarName, n: Int): Compilation[Query] = ??? // do an upto with a precomputed view

  def doFind(findable: UnsafeFindable): Compilation[Query] = ??? // returns SQL code to do a find of a findable
}