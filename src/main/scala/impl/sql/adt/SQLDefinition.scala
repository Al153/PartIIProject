package impl.sql.adt

import core.intermediate.unsafe.ErasedRelationAttributes
import impl.sql.adt.SQLPair
import impl.sql.compile.{RelName, VarName}

sealed trait SQLDefinition {
  def bodyQuery: String
  def nameString: String
}

case class PairDefinition(name: VarName, body: SQLPair) extends SQLDefinition {
  override def bodyQuery: String =  SQLPair.getQueryString(body)
  override def nameString: String = name.s
}

case class RecursiveDefinition(name: VarName, body: SQLPair) extends SQLDefinition {
  
}

case class SingleDefinition(name: VarName, body: SQLSingle) extends SQLDefinition {
  override def bodyQuery: String = SQLSingle.getQueryString(body)

  override def nameString: String = name.s
}

case class RelationDefinition(name: RelName, rel: ErasedRelationAttributes) extends SQLDefinition {
  override def bodyQuery: String = ??? // need to do a join with commit id
  override def nameString: String = name.s
}

object SQLDefinition {
  def getQuery(defn: SQLDefinition): String = defn.bodyQuery
  def createDef(name: VarName, q: SQLPair): SQLDefinition = PairDefinition(name, q)
  def createDef(name: VarName, q: SQLSingle): SQLDefinition = SingleDefinition(name, q)
  def createDef(name: RelName, rel: ErasedRelationAttributes): SQLDefinition = RelationDefinition(name, rel)
}