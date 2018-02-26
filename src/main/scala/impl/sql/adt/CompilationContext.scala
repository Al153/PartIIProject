package impl.sql.adt

import core.backend.intermediate.unsafe.ErasedRelationAttributes
import core.user.schema.TableName
import core.utils._
import impl.sql.tables.{AuxObjectTable, ObjectTable, RelationTable}
import impl.sql.{SQLEither, SQLInstance}

import scalaz.State


/**
  * Compilation context used in a state monad to store all the required relations, tables, auxtables
  * that they may be aliased and filtered for the values relevant to a view
  * @param varCount - counter for var names. ensures uniqueness
  * @param relations - aliases for relation tables
  * @param requiredTables - aliases for the tables used by the compilation
  * @param requiredAuxTables - aliases for the [[AuxObjectTable]]s needed
  * @param commonSubExpressions - Common sub expressions that have bee factored out and given a name
  *
  * The main reason this class is needed is that PostgreSQL does not allow nested CTEs (WITH ... AS ... (...),
  * effectively, Let). So we need to have all definitions at the top level in a single CTE,
  * some this class stores everything that needs to be defined
  */
case class CompilationContext(
                               varCount: Int,
                               relations: Map[ErasedRelationAttributes, VarName],
                               requiredTables: Map[TableName, VarName], // tables needed for query.
                               requiredAuxTables: Map[TableName, VarName], // the aux (commit) tables needed for the query
                               commonSubExpressions: Map[SubExpression, VarName]
                             ) {
  /**
    * Create a new, unique var name
    */
  def newVarName: (CompilationContext, VarName) =
    (CompilationContext(varCount + 1, relations, requiredTables, requiredAuxTables, commonSubExpressions), VarName("Var" + varCount))

  /**
    * Lookup or get a new alias for a relation table
    */
  def getRelationName(r: ErasedRelationAttributes): (CompilationContext, VarName)  =
    if (r in relations) (this, relations(r))
    else {
      val newName = VarName("Relation"  + varCount )
      (CompilationContext(varCount + 1, relations + (r -> newName), requiredTables, requiredAuxTables, commonSubExpressions), newName)
    }

  /**
    * Lookup or get a new alias for an object table
    */
  def getTableName(name: TableName): (CompilationContext, VarName) =
    if (name in requiredTables) (this, requiredTables(name))
    else {
      val newName = VarName("Table"  + varCount )
      (CompilationContext(varCount + 1, relations, requiredTables + (name -> newName), requiredAuxTables, commonSubExpressions), newName)
    }

  /**
    * Lookup or get a new alias for an auxialliary table
    */
  def getAuxTable(name: TableName): (CompilationContext, VarName) =
    if (name in requiredAuxTables) (this, requiredAuxTables(name))
    else {
      val newName = VarName("AuxTable"  + varCount )
      (CompilationContext(varCount + 1, relations, requiredTables, requiredAuxTables + (name -> newName), commonSubExpressions), newName)
    }

  /**
    * define a "WITH x as ..."
    */
  def newCommonSubExpression(q: Query): (CompilationContext, VarName) =
    // implements CSE
    if (SimpleSubExpr(q) in commonSubExpressions) (this, commonSubExpressions(SimpleSubExpr(q)))
    else {
      val newName = VarName("View" + varCount)
      (CompilationContext(varCount + 1, relations, requiredTables, requiredAuxTables, commonSubExpressions + (SimpleSubExpr(q) -> newName)), newName)
    }

  /**
    * Define a new recursive common subexpression
    */
  def newRecursive(f: VarName => Query): (CompilationContext, VarName) = {
    val newName = VarName("View" + varCount)
    (CompilationContext(varCount + 1, relations, requiredTables, requiredAuxTables, commonSubExpressions + (Recursive(f(newName)) -> newName)), newName)
  }


  private def getRelationDefs: Map[ErasedRelationAttributes, VarName] = relations // idea: get definitions of relations
  private def getTableDefs: Map[TableName, VarName] = requiredTables // ditto but for object tables
  private def getAuxDefs: Map[TableName, VarName] = requiredAuxTables // ditto

  /**
    * Gets all the definitions in this context
    * @param instance
    * @return
    */
  def getDefs(
               instance: SQLInstance
             ): SQLEither[(Iterable[(ObjectTable, VarName)], Iterable[(AuxObjectTable, VarName)], Iterable[(RelationTable, VarName)])] =  for {
    tableDefs <- EitherOps.sequence(
      for {
        (name, sqlName) <- getTableDefs
      } yield instance.lookupTable(name).withSnd(sqlName))

    auxTableDefs <- EitherOps.sequence(
      for {
        (name, sqlName) <- getAuxDefs
      } yield instance.lookupTable(name).map(_.auxTable).withSnd(sqlName)
    )

    relationDefs <- EitherOps.sequence(for {
      (rel, sqlName) <- getRelationDefs
    } yield instance.lookupRelation(rel).withSnd(sqlName))
  } yield (tableDefs, auxTableDefs, relationDefs)
}


object CompilationContext {
  /**
    * Monadic compilation
    */
  type Compilation[A] = State[CompilationContext, A]

  /**
    * Monadically get a new VarName
    */
  def newSymbol: Compilation[VarName] = State {
    initial: CompilationContext =>
      initial.newVarName
  }

  /**
    * Monadically lookup or get a new alias for a relation table
    */
  def getRelationName(r: ErasedRelationAttributes): Compilation[VarName] = State {
    initial: CompilationContext =>
      initial.getRelationName(r)
  }

  /**
    * monadically Lookup or get a new alias for an object table
    */
  def getTableName(name: TableName): Compilation[VarName] = State {
    initial: CompilationContext =>
      initial.getTableName(name)
  }

  /**
    * Lookup or get a new alias for an auxialiary table
    */
  def getAuxTableName(name: TableName): Compilation[VarName] = State {
    initial: CompilationContext =>
      initial.getAuxTable(name)
  }

  /**
    * Monadically define a "WITH x as ..."
    */
  def newSubexpression(q: Query): Compilation[VarName] = State {
    initial: CompilationContext =>
      initial.newCommonSubExpression(q)
  }

  /**
    * Fixed point function to create a recursive query
     * @param f
    * @return
    */

  def fixedPoint(f: VarName => Query): Compilation[VarName] = State {
    initial: CompilationContext =>
      initial.newRecursive(f)
  }

  /**
    * Lift a value into a compilation
    * @param a
    * @tparam A
    * @return
    */
  def point[A](a: => A): Compilation[A] = State(s => (s, a))
}
