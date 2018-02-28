package impl.sql.adt

import core.backend.intermediate.unsafe._
import core.user.dsl.ViewId
import core.user.schema.TableName
import core.utils.Logged
import impl.sql.SQLInstance
import impl.sql.adt.CompilationContext.Compilation
import impl.sql.adt.queries._
import impl.sql.names.SQLColumnName

/**
  * The result of rendering every exposes a left_id and right_id to allow composability
  */
sealed trait Query

/**
  * Simply use an Alias
  */
case class Var private (v: VarName) extends Query

/**
  * spacing Alias, shouldn't be used manually
  *
  * ... AS ...
  *
  * Used to solve PostgreSQL syntax errors
  */
case class Alias private(name: VarName, query: Query) extends Query

/**
  * SELECT [[mappings]] FROM [[where]] WHERE [[where]]
  */
case class SelectWhere private (mappings: SelectMapping, where: Where, from: Query) extends Query

/**
  * SELECT id as left_id, id as right_id FROM [[tableName]] WHERE [[where]]
  */
case class SelectTable private (tableName: VarName, auxTable: VarName, where: WhereTable) extends Query

/**
  * [[left]] INTERSECT [[right]]
  */
case class IntersectAll private (left: Query, right: Query) extends Query
/**
  * [[left]] UNION ALL [[right]]
  */
case class UnionAll private (left: Query, right: Query) extends Query

/**
  * [[left]] UNION [[right]]
  */
case class Union private (left: Query, right: Query) extends Query

/**
  * Join two queries, renaming the queries with Aliases
  */
case class JoinRename private (leftMapping: (VarName, Query), rightMapping: (VarName, Query), on: JoinMapping) extends Query // ($left as $asLeft) join ($right as $asRight) on $on

/**
  * Join where the left and right are varnames
  */
case class JoinSimple private (left: VarName, right: VarName, on: JoinMapping) extends Query // $left join $right on Mapping

object Query extends Logged {
  /**
    * Default [[CompilationContext]]
    */
  def emptyContext: CompilationContext = new CompilationContext(0, Map(), Map(), Map(), Map())

  /**
    * Remove over-zealous aliasing
    */
  private def removeAliases(q: Query): Query =
    q match {
      case Alias(_, q1) => removeAliases(q1)
      case _ => q
    }

  /**
    * Render a query to a string using simple case matching
    */
  def render(q: Query): String = q match {
    case Var(v) => v.s
    case Alias(v, sub) => sub match {
      case Alias(_, _) => render(sub)
      case _ => s"(${render(sub)}) AS ${v.s}"
    }

    case SelectTable(name, auxTable, where) =>
      s"SELECT ${SelectMapping.render(FromObject)} FROM ($name JOIN $auxTable ON $name.${SQLColumnName.objId} = $auxTable.${SQLColumnName.leftId}) ${WhereTable.render(where)}"
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

  /**
    * Recursive monadic compilation of a FindPair.
    *
    * Fairly straight-forward subcases
    */
  def convertPair(q: UnsafeFindPair): Compilation[Query] = q match  {
    // simply return intersection
    case USAnd(left, right) => for {
      l <- convertPair(left)
      r <- convertPair(right)
    } yield IntersectAll(l, r)

    // simply return intersection on right hand side, using a join and a pair of aliases
    case USAndRight(left, right) => for {
      l <- convertPair(left)
      l1 <- optionalAlias(l)
      r <- convertSingle(right)
      r1 <- optionalAlias(r)
      a <- CompilationContext.newSymbol
      b <- CompilationContext.newSymbol
    } yield SelectWhere(SameSide(a), NoConstraint, JoinRename(a -> l1, b -> r1, OnRight))

    // simply return intersection on left hand side, using a join and a pair of aliases
    case USAndLeft(left, single) => for {
      l <- convertPair(left)
      l1 <- optionalAlias(l)
      r <- convertSingle(single)
      r1 <- optionalAlias(r)
      a <- CompilationContext.newSymbol
      b <- CompilationContext.newSymbol
    } yield SelectWhere(SameSide(a), NoConstraint, JoinRename(a -> l1, b -> r1, OnLeft))

    // an atleast requires a fixed point (recursive) subexpression
    case USFixedPoint(rel) => for {
      precomputed <- convertPair(rel) // precompute a view
      temporaryView <- CompilationContext.newSubexpression(precomputed) // get a name for it
      n <- CompilationContext.getAuxTableName(rel.leftMostTable)
      // compute as an atleast(0) using a fixed point
      rec <- CompilationContext.fixedPoint {
        recursiveCall =>
          Union(
            SelectWhere(Simple, NoConstraint, Var(n)),
            SelectWhere( // add values to it
              Joined(recursiveCall, temporaryView),
              NoConstraint,
              JoinSimple(recursiveCall, temporaryView, Chained))
          )
      }
    } yield SelectWhere(Simple, NoConstraint, Var(rec))

    // Chains are implemented using a join
    case USChain(left, right) => for {
      l <- convertPair(left)
      r <- convertPair(right)
      l1 <- optionalAlias(l)
      r1 <- optionalAlias(r)
      a <- CompilationContext.newSymbol
      b <- CompilationContext.newSymbol
    } yield SelectWhere(Joined(a, b), NoConstraint, JoinRename(a -> l1, b -> r1, Chained))

      // Simply wrap a SELECT left_id, right_id FROM .. WHERE left_id == right_id
    case USDistinct(rel) =>  for {
      r <- convertPair(rel)
      r1 <- optionalAlias(r)
    } yield SelectWhere(Simple, Distinct, r1)

      // Delegate an exactly
    case USExactly(n, rel) => for {
      view <- convertPair(rel)
      viewName <- CompilationContext.newSubexpression(view)
      body <- getExactly(viewName, n, rel.leftMostTable, rel.leftMostTable)
    } yield body

      // id, select all values from an object
    case USId(tableName) => for {
      n <- CompilationContext.getAuxTableName(tableName)
    } yield SelectWhere(FromObject, NoConstraint, Var(n))

      // simply union results together
    case USOr(left, right) =>  for {
      l <- convertPair(left)
      r <- convertPair(right)
    } yield UnionAll(l, r)


      // An alias for each used relation is stored in the context, so we just select from the alias
    case USRel(rel) => for {
      r <- CompilationContext.getRelationName(rel)
    } yield SelectWhere(All, NoConstraint, Var(r))

      // same as USRel
    case USRevRel(rel) => for {
      r <- CompilationContext.getRelationName(rel)
    } yield SelectWhere(ReversedRelation(r), NoConstraint, Var(r))

      // upto delegates role to a helper method
    case USUpto(n, rel) => for {
      view <- convertPair(rel)
      viewName <- CompilationContext.newSubexpression(view)
      body <- getUpto(viewName, n, rel.leftMostTable)
    } yield body
  }

  /**
    * Recursive monadic compilation of a FindSingle.
    *
    * Fairly straight forward subcases
    *
    * Result of a single query is in the right_id column
    */
  def convertSingle(q: UnsafeFindSingle): Compilation[Query] = q match {
      // delegate
    case USFind(findable) => doFind(findable)

    // similar to a join in the pair, pair case
    case USFrom(start, rel) => for {
      a <- CompilationContext.newSymbol
      b <- CompilationContext.newSymbol
      start <- convertSingle(start)
      start1 <- optionalAlias(start)
      rel <- convertPair(rel)
      rel1 <- optionalAlias(rel)
    } yield SelectWhere(Joined(a, b), NoConstraint, JoinRename(a -> start1, b -> rel1, Chained))

    // Mirrors AndPair
    case USAndS(l, r) => for {
      left <- convertSingle(l)
      right <- convertSingle(r)
    } yield IntersectAll(left, right)


    case USOrS(l, r) => for {
      left <- convertSingle(l)
      right <- convertSingle(r)
    } yield UnionAll(left, right)


  }

  /**
    * Helper method for getting queries to do exactlies
    * @param precomputed - CTE for doing the lookup
    * @param n - number of instances of the relation to self join
    * @param emptyTable - table to extract all from in the case n = 0
    * @return
    */
  private def getExactly(precomputed: VarName, n: Int, emptyTable: TableName, emptyRelationTable: TableName): Compilation[Query] = {
    // idea: inspired by binary multiplication
    def generateJoins(n: Int, x: Query, acc: Query): Compilation[Query] =
      if (n <= 0) CompilationContext.point(acc)
      else
        for {
          a <- CompilationContext.newSymbol
          b <- CompilationContext.newSymbol
          r <- generateJoins(n-1, x, SelectWhere(
            Joined(a, b),
            NoConstraint,
            JoinRename(a -> x, b -> acc, Chained)
          ))
        } yield r
    for {
      e <- allFrom(emptyTable)
      res <- generateJoins(n, Var(precomputed), e)
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

  /**
    * Get a query for all values from a given table in a given view
    */
  private def allFrom(table: TableName): Compilation[Query] = for {
    auxTable <- CompilationContext.getAuxTableName(table)
  } yield Var(auxTable)


  /**
    * Find all values in a table that match a given findable in the view
    * @param findable - pattern to find
    * @return
    */

  private def doFind(findable: ErasedFindable): Compilation[Query] = for {
    name <- CompilationContext.getTableName(findable.tableName)
    // needs to consult aux table to get values in the required view
    aux <- CompilationContext.getAuxTableName(findable.tableName)
  } yield SelectTable(name, aux, Pattern(findable)) // returns SQL code to do a find of a findable
}