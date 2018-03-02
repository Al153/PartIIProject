package core.user.dsl

import core.backend.intermediate._
import core.user.schema.{SchemaObject, TableName}

import scala.language.implicitConversions

/**
  * Created by Al on 15/10/2017.
  *
  * A [[Relation]] is a relation in the DB
  */
abstract class Relation[A, B](implicit val sa: SchemaObject[A], val sb: SchemaObject[B]) extends FindPairAble[A, B]
{
  /**
    * Names of tables used by the query
    */
  def tableNames: (TableName, TableName) = (sa.name, sb.name)

  /**
    *
    */

  final override def toFindPair: FindPair[A, B] = Rel(this)
}



