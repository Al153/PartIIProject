package impl.sql.adt

import core.intermediate.unsafe._
import core.schema.TableName
import impl.sql.adt.CompilationContext.Compilation

/**
  * A query should expose a left_id and right_id to allow composability
  */
sealed trait Query
case class With private (defn:(VarName, Query), in: Query) extends Query
case class WithView private (name: VarName, body: Query, in: Query) extends Query // needs to drop view
case class WithRec private (name: VarName, body: Query, in: Query) extends Query
case class Var private (v: VarName) extends Query
case class SelectWhere private (mappings: SelectMapping, where: Where, from: Query) extends Query
case class SelectTable private (tableName: VarName, where: WhereTable) extends Query
case class IntersectAll private (left: Query, right: Query) extends Query
case class UnionAll private (left: Query, right: Query) extends Query
case class JoinRename private (leftMapping: (VarName, Query), rightMapping: (VarName, Query), on: JoinMapping) extends Query // ($left as $asLeft) join ($right as $asRight) on $on
case class JoinSimple private (left: VarName, right: VarName, on: JoinMapping) extends Query // $left join $right on Mapping

object Query {
  def emptyContext: CompilationContext = new CompilationContext(0, Map(), Map())

  def convert(q: UnsafeFindPair): Compilation[Query] = q match  {
    case USAnd(left, right) => for {
      l <- convert(left)
      r <- convert(right)
    } yield IntersectAll(l, r)

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
      preTraversal <- getExactly(temporaryView, n, rel.leftMostTable) // computer the traversal up to the start of the upto
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
      preTraversal <- getExactly(viewName, low, rel.leftMostTable)
      preTraversalName <- CompilationContext.newVarName
      postTraversal <- getUpto(viewName, high-low, rel.leftMostTable)
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
      body <- getExactly(viewName, n, rel.leftMostTable)
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
      body <- getUpto(viewName, n, rel.leftMostTable)
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

  private def getExactly(precomputed: VarName, n: Int, emptyTableName: TableName): Compilation[Query] = {
    // idea: inspired by binary multiplication
    def joinBySquares(n: Int, x: Query, emptyQuery: Query): Compilation[Query] =
      if (n <= 0) CompilationContext.point(emptyQuery)
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
            ), emptyQuery
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
            ), emptyQuery
          )
          l <- CompilationContext.newVarName
          r <- CompilationContext.newVarName
        } yield SelectWhere(Joined(l, r), NoConstraint, JoinRename(l -> x, r -> doubled, Chained))

    for {
      e <- allFrom(emptyTableName)
      res <- joinBySquares(n, Var(precomputed), e)
    } yield res

  }

  private def getUpto(precomputed: VarName, n: Int, emptyRelationTable: TableName): Compilation[Query] = {
    // do an upto with a precomputed view
    for {
      recName <- CompilationContext.newVarName
      lim <- CompilationContext.newVarName
      empty <- allFrom(emptyRelationTable)
    } yield WithRec(recName, // create a recursive query
      UnionAll(
        SelectWhere(StartLimit(lim, All), NoConstraint, empty), // basis case: pretraversal
        SelectWhere( // add values to it
          Joined(recName, precomputed),
          Limit(lim, n),
          JoinSimple(recName, precomputed, Chained))
      ),
      SelectWhere(Simple, NoConstraint, Var(recName))
    )
  }

  private def allFrom(tableName: TableName): Compilation[Query] = for {
    //  todo: this needs to go by commits
    tVar <- CompilationContext.getTableName(tableName)
  } yield SelectWhere(FromObject, NoConstraint, Var(tVar))

  private def doFind(findable: UnsafeFindable): Compilation[Query] = for {
    name <- CompilationContext.getTableName(findable.tableName)
  } yield SelectTable(name, Pattern(findable)) // returns SQL code to do a find of a findable
}