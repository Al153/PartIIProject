package impl.sql.adt

import core.backend.intermediate.unsafe.ErasedRelationAttributes
import core.user.schema.TableName
import core.utils._
import impl.sql.tables.{AuxObjectTable, ObjectTable, RelationTable}
import impl.sql.{SQLEither, SQLInstance}

import scalaz.State



case class CompilationContext(
                               varCount: Int,
                               relations: Map[ErasedRelationAttributes, VarName],
                               requiredTables: Map[TableName, VarName], // tables needed for query.
                               requiredAuxTables: Map[TableName, VarName], // the aux (commit) tables needed for the query
                               commonSubExpressions: Map[SubExpression, VarName]
                             ) {

  def newVarName: (CompilationContext, VarName) =
    (CompilationContext(varCount + 1, relations, requiredTables, requiredAuxTables, commonSubExpressions), VarName("Var" + varCount))

  def getRelationName(r: ErasedRelationAttributes): (CompilationContext, VarName)  =
    if (r in relations) (this, relations(r))
    else {
      val newName = VarName("Relation"  + varCount )
      (CompilationContext(varCount + 1, relations + (r -> newName), requiredTables, requiredAuxTables, commonSubExpressions), newName)
    }

  def getTableName(name: TableName): (CompilationContext, VarName) =
    if (name in requiredTables) (this, requiredTables(name))
    else {
      val newName = VarName("Table"  + varCount )
      (CompilationContext(varCount + 1, relations, requiredTables + (name -> newName), requiredAuxTables, commonSubExpressions), newName)
    }

  def getAuxTable(name: TableName): (CompilationContext, VarName) =
    if (name in requiredAuxTables) (this, requiredAuxTables(name))
    else {
      val newName = VarName("AuxTable"  + varCount )
      (CompilationContext(varCount + 1, relations, requiredTables, requiredAuxTables + (name -> newName), commonSubExpressions), newName)
    }

  def newCommonSubExpression(q: Query): (CompilationContext, VarName) =
    if (SimpleSubExpr(q) in commonSubExpressions) (this, commonSubExpressions(SimpleSubExpr(q)))
    else {
      val newName = VarName("View" + varCount)
      (CompilationContext(varCount + 1, relations, requiredTables, requiredAuxTables, commonSubExpressions + (SimpleSubExpr(q) -> newName)), newName)
    }

  def newRecursive(f: VarName => Query): (CompilationContext, VarName) = {
    val newName = VarName("View" + varCount)
    (CompilationContext(varCount + 1, relations, requiredTables, requiredAuxTables, commonSubExpressions + (Recursive(f(newName)) -> newName)), newName)
  }

  private def getRelationDefs: Map[ErasedRelationAttributes, VarName] = relations // idea: get definitions of relations
  private def getTableDefs: Map[TableName, VarName] = requiredTables // ditto but for object tables
  private def getAuxDefs: Map[TableName, VarName] = requiredAuxTables

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
  type Compilation[A] = State[CompilationContext, A]

  def newSymbol: Compilation[VarName] = State {
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

  def getAuxTableName(name: TableName): Compilation[VarName] = State {
    initial: CompilationContext =>
      initial.getAuxTable(name)
  }

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

  def point[A](a: => A): Compilation[A] = State(s => (s, a))
}
