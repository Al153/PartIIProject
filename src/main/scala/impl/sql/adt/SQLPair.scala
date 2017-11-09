package impl.sql.adt

import core.schema.TableName
import impl.sql.compile.RelName
import impl.sql.tables.RelationTable

// idea: execution of an SQL pair should return a pair of ids

sealed trait SQLPair // lookup for pairs of ids
case class JoinRelation(relation: RelName) extends SQLPair
case class JoinReverseRelation(relation: RelName) extends SQLPair
case class Variable(name: String) extends SQLPair
case class Chain(left: SQLPair, right: SQLPair) extends SQLPair
case class Intersection(left: SQLPair, right: SQLPair) extends SQLPair
case class Union(left: SQLPair, right: SQLPair) extends SQLPair
case class Narrow(left: SQLPair, right: SQLSingle) extends SQLPair
case class Id(tableName: TableName) extends SQLPair
case class Distinct(r: SQLPair) extends SQLPair
case class Recursive(r: SQLRecursive) extends SQLPair

sealed trait SQLRecursive // repeatedly apply an SQLPair
case class Exactly(n: Int, rel: SQLPair) extends SQLRecursive
case class Upto(n: Int, rel: SQLPair) extends SQLRecursive
case class Atleast(n: Int, rel: SQLPair) extends SQLRecursive
case class Between(low: Int, high: Int, rel: SQLPair) extends SQLRecursive



object SQLPair {
  def getQueryString(p: SQLPair): String = ???
}



