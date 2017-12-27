package impl.sql.adt

import core.intermediate.unsafe._
import core.schema.TableName
import impl.sql.adt.CompilationContext.Compilation
import queries._

/**
  * A query should expose a left_id and right_id to allow composability
  */
sealed trait Query
case class Var private (v: VarName) extends Query
case class Alias private(name: VarName, query: Query) extends Query // spacing Alias, shouldn't be used seriously
case class SelectWhere private (mappings: SelectMapping, where: Where, from: Query) extends Query
case class SelectTable private (tableName: VarName, where: WhereTable) extends Query
case class IntersectAll private (left: Query, right: Query) extends Query
case class UnionAll private (left: Query, right: Query) extends Query
case class Union private (left: Query, right: Query) extends Query
case class JoinRename private (leftMapping: (VarName, Query), rightMapping: (VarName, Query), on: JoinMapping) extends Query // ($left as $asLeft) join ($right as $asRight) on $on
case class JoinSimple private (left: VarName, right: VarName, on: JoinMapping) extends Query // $left join $right on Mapping

object Query {
  def emptyContext: CompilationContext = new CompilationContext(0, Map(), Map(), Map())

  private def removeAliases(q: Query): Query =
    q match {
      case Alias(_, q1) => removeAliases(q1)
      case _ => q
    }

  def render(q: Query): String = q match {
    case Var(v) => v.s
    case Alias(v, sub) => sub match {
      case Alias(_, _) => render(sub)
      case _ => s"(${render(sub)}) AS ${v.s}"
    }
    case SelectTable(name, where) => s"SELECT ${SelectMapping.render(FromObject)} FROM $name ${WhereTable.render(where)}"
    case SelectWhere(mappings, where, from) => s"SELECT ${SelectMapping.render(mappings)} FROM ${render(from)} ${Where.render(where)}"
    case IntersectAll(left, right) => s"(${render(left)}) INTERSECT (${render(right)})"
    case UnionAll(left, right) => s"(${render(left)}) UNION ALL (${render(right)})"
    case Union(left, right) => s"(${render(left)}) UNION ${render(right)}"
    case JoinRename((asLeft, left), (asRight, right), on) =>
      val newLeft = removeAliases(left)
      val newRight = removeAliases(right)
      s"${optionalBrackets(render(newLeft))} AS $asLeft INNER JOIN ${optionalBrackets(render(newRight))} AS $asRight ON ${JoinMapping.render(on, asLeft, asRight)}"
    case JoinSimple(l, r, on) => s"$l INNER JOIN $r ON ${JoinMapping.render(on, l, r)}"
  }

  def convertPair(q: UnsafeFindPair): Compilation[Query] = q match  {
    case USAnd(left, right) => for {
      l <- convertPair(left)
      r <- convertPair(right)
    } yield IntersectAll(l, r)

    case USAndSingle(left, right) => for {
      l <- convertPair(left)
      l1 <- optionalAlias(l)
      r <- convertSingle(right)
      r1 <- optionalAlias(r)
      a <- CompilationContext.newSymbol
      b <- CompilationContext.newSymbol
    } yield SelectWhere(SameSide(a), NoConstraint, JoinRename(a -> l1, b -> r1, OnRight))

    case USAtleast(n , rel) => for {
      precomputed <- convertPair(rel) // precompute a view
      temporaryView <- CompilationContext.newSubexpression(precomputed) // get a name for it

      preTraversal <- getExactly(temporaryView, n, rel.leftMostTable) // computer the traversal up to the start of the upto
      preTraversalName <- CompilationContext.newSubexpression(preTraversal) // give it a name

      rec <- CompilationContext.fixedPoint {
        recursiveCall =>
          Union(
            SelectWhere(Simple, NoConstraint, Var(preTraversalName)), // basis case: pretraversal
            SelectWhere( // add values to it
              Joined(recursiveCall, temporaryView),
              NoConstraint,
              JoinSimple(recursiveCall, temporaryView, Chained))
          )
      }
    } yield SelectWhere(Simple, NoConstraint, Var(rec))
    case USBetween(low, high, rel) => for {
      view <- convertPair(rel)
      viewName <- CompilationContext.newSubexpression(view)
      preTraversal <- getExactly(viewName, low, rel.leftMostTable)
      preTraversalName <- CompilationContext.newSymbol
      postTraversal <- getUpto(viewName, high-low, rel.leftMostTable)
      postTraversalName <- CompilationContext.newSymbol
    } yield SelectWhere(
      Joined(preTraversalName, postTraversalName),
      NoConstraint,
      JoinRename(
        preTraversalName -> preTraversal,
        postTraversalName -> postTraversal,
        Chained
      )
    )

    case USChain(left, right) => for {
      l <- convertPair(left)
      r <- convertPair(right)
      l1 <- optionalAlias(l)
      r1 <- optionalAlias(r)
      a <- CompilationContext.newSymbol
      b <- CompilationContext.newSymbol
    } yield SelectWhere(Joined(a, b), NoConstraint, JoinRename(a -> l1, b -> r1, Chained))

    case USDistinct(rel) =>  for {
      r <- convertPair(rel)
      r1 <- optionalAlias(r)
    } yield SelectWhere(Simple, Distinct, r1)

    case USExactly(n, rel) => for {
      view <- convertPair(rel)
      viewName <- CompilationContext.newSubexpression(view)
      body <- getExactly(viewName, n, rel.leftMostTable)
    } yield body

    case USId(tableName) => for {
      n <- CompilationContext.getTableName(tableName)
    } yield SelectWhere(Simple, NoConstraint, Var(n))

    case USNarrow(rel, findable) => for {
      l <- convertPair(rel)
      r <- doFind(findable)
      l1 <- optionalAlias(l)
      r1 <- optionalAlias(r)
      a <- CompilationContext.newSymbol
      b <- CompilationContext.newSymbol
    } yield SelectWhere(SameSide(a), NoConstraint, JoinRename(a -> l1, b -> r1, OnRight))

    case USOr(left, right) =>  for {
      l <- convertPair(left)
      r <- convertPair(right)
    } yield UnionAll(l, r)

    case USRel(rel) => for {
      r <- CompilationContext.getRelationName(rel)
    } yield SelectWhere(All, NoConstraint, Var(r))

    case USRevRel(rel) => for {
      r <- CompilationContext.getRelationName(rel)
    } yield SelectWhere(ReversedRelation(r), NoConstraint, Var(r))

    case USUpto(n, rel) => for {
      view <- convertPair(rel)
      viewName <- CompilationContext.newSubexpression(view)
      body <- getUpto(viewName, n, rel.leftMostTable)
    } yield body
  }

  def convertSingle(q: UnsafeFindSingle): Compilation[Query] = q match {
    case USFind(findable) => doFind(findable)

    case USFrom(start, rel) => for {
      a <- CompilationContext.newSymbol
      b <- CompilationContext.newSymbol
      start <- convertSingle(start)
      start1 <- optionalAlias(start)
      rel <- convertPair(rel)
      rel1 <- optionalAlias(rel)
    } yield SelectWhere(Joined(a, b), NoConstraint, JoinRename(a -> start1, b -> rel1, Chained))


    case USNarrowS(start, findable) => for {
      a <- CompilationContext.newSymbol
      b <- CompilationContext.newSymbol
      start <- convertSingle(start)
      start1 <- optionalAlias(start)
      filter <- doFind(findable)
      filter1 <- optionalAlias(filter)
    } yield SelectWhere(SameSide(a), NoConstraint, JoinRename(a -> start1, b -> filter1, OnRight))

  }

  private def getExactly(precomputed: VarName, n: Int, emptyTableName: TableName): Compilation[Query] = {
    // idea: inspired by binary multiplication
    def joinBySquares(n: Int, x: Query, emptyQuery: Query): Compilation[Query] =
      if (n <= 0) CompilationContext.point(emptyQuery)
      else if (n <= 1) CompilationContext.point(x)
      else if (n % 2 == 0)
        for {
          a <- CompilationContext.newSymbol
          b <- CompilationContext.newSymbol
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
          a <- CompilationContext.newSymbol
          b <- CompilationContext.newSymbol
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
          l <- CompilationContext.newSymbol
          r <- CompilationContext.newSymbol
        } yield SelectWhere(Joined(l, r), NoConstraint, JoinRename(l -> x, r -> doubled, Chained))

    for {
      e <- allFrom(emptyTableName)
      res <- joinBySquares(n, Var(precomputed), e)
    } yield res

  }

  /**
    * Finds pairs linked by upto n traversals of the precomputed table
    * @param precomputed - a variable representing the precomputed table
    * @param n - max number of repetitions
    * @param emptyRelationTable - tale to use for the empty relations
    * @return
    */
  private def getUpto(precomputed: VarName, n: Int, emptyRelationTable: TableName): Compilation[Query] = {
    for {
      limit <- CompilationContext.newSymbol
      baseCase <- allFrom(emptyRelationTable)
      baseCase1 <- optionalAlias(baseCase)

      recName <- CompilationContext.fixedPoint {
        recursiveCall =>
          Union(
            SelectWhere(StartLimit(limit, All), NoConstraint, baseCase1), // basis case: pretraversal
            SelectWhere( // add values to it
              WithLimit(limit, Joined(recursiveCall, precomputed)),
              Limit(limit, n),
              JoinSimple(recursiveCall, precomputed, Chained))
          )
      }
    } yield SelectWhere(Simple, NoConstraint, Var(recName))
  }

  private def allFrom(tableName: TableName): Compilation[Query] = for {
    tVar <- CompilationContext.getTableName(tableName)
  } yield SelectWhere(FromObject, NoConstraint, Var(tVar))

  private def doFind(findable: UnsafeFindable): Compilation[Query] = for {
    name <- CompilationContext.getTableName(findable.tableName)
  } yield SelectTable(name, Pattern(findable)) // returns SQL code to do a find of a findable
}