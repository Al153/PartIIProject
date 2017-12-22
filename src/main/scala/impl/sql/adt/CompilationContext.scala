package impl.sql.adt

import core.intermediate.unsafe.ErasedRelationAttributes
import core.schema.TableName
import core.utils._

import scalaz.State

case class CompilationContext(
                               varCount: Int,
                               relations: Map[ErasedRelationAttributes, VarName],
                               requiredTables: Map[TableName, VarName], // tables needed for query.
                               commonSubExpressions: Map[Query, VarName]
                             ) {

  def newVarName: (CompilationContext, VarName) = (CompilationContext(varCount + 1, relations, requiredTables, commonSubExpressions), VarName("Var" + varCount))

  def getRelationName(r: ErasedRelationAttributes): (CompilationContext, VarName)  =
    if (r in relations) (this, relations(r))
    else {
      val newName = VarName("Relation"  + varCount )
      (CompilationContext(varCount + 1, relations + (r -> newName), requiredTables, commonSubExpressions), newName)
    }

  def getTableName(name: TableName): (CompilationContext, VarName) =
    if (name in requiredTables) (this, requiredTables(name))
    else {
      val newName = VarName("Table"  + varCount )
      (CompilationContext(varCount + 1, relations, requiredTables + (name -> newName), commonSubExpressions), newName)
    }

  def newCommonSubExpression(q: Query): (CompilationContext, VarName) =
    if (q in commonSubExpressions) (this, commonSubExpressions(q))
    else {
      val newName = VarName("View" + varCount)
      (CompilationContext(varCount + 1, relations, requiredTables, commonSubExpressions + (q -> newName)), newName)
    }

  def getRelationDefs: Map[ErasedRelationAttributes, VarName] = relations // idea: get definitions of relations
  def getTableDefs: Map[TableName, VarName] = requiredTables // ditto but for object tables
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

  def newView(q: Query): Compilation[VarName] = State {
    initial: CompilationContext =>
      initial.newCommonSubExpression(q)
  }

  def point[A](a: => A): Compilation[A] = State(s => (s, a))
}
