package impl.sql.errors

import java.sql.SQLException

import core.backend.common.{ExtractError, MissingRelation}
import core.backend.intermediate.unsafe.ErasedRelationAttributes
import core.user.dsl.E
import impl.sql.names.SQLColumnName
import impl.sql.schema.{ColumnSpecification, SQLType}

/**
  * Sealed trait hierarchy
  */

sealed trait SQLError extends E

/**
  * Contains an exception caught from the JDBC interface
  */

case class CaughtSQLException(e: SQLException) extends SQLError {
  override def toString: String = "CAUGHT SQL EXCEPTION: " + e.getMessage + "\n" + e.getStackTrace.mkString("\n") +
    (if (e.getCause != null)"\n" + e.getCause.toString else "")
}

/**
  * Appears when the result of an SQL query is unexpectedly empty
  */

case class EmptyResultError(query: String) extends SQLError

/**
  * Occurs when the [[impl.sql.tables.DefaultsTable]] is empty
  */

object MissingDefaultViewError extends SQLError


/**
  * Occurs when the user tries to use an unknown relation
  */
case class SQLErasedRelationMissing(r: ErasedRelationAttributes) extends SQLError


/**
  * Wraps an extract error when extracting values from the database
  */
case class SQLExtractError(e: ExtractError) extends SQLError {
  override def toString: String = e.toString
}

/**
  * Created by Al on 19/12/2017.
  *
  * Occurs when there is a missing relation error when converting a query to unsafe
  */
case class SQLMissingRelation(e: MissingRelation) extends SQLError {

}
/**
  * Created by Al on 11/12/2017.
  *
  * Sealed trait hierarchy of errors due to mismatchwed between the expected schema and the actual schema in
  * the PostgreSQL DB
  */
sealed trait SQLSchemaMismatch extends SQLError
case class SQLSchemaLengthMismatch() extends SQLSchemaMismatch
case class SQLSchemaColumnMissing(column: ColumnSpecification, expected: Map[SQLColumnName, SQLType]) extends SQLSchemaMismatch
case class SQLSchemaTypeCheckError(expected: SQLType, actual: String) extends SQLSchemaMismatch
case class SQLSchemaUnexpectedType(s: String) extends SQLSchemaMismatch

/**
  * Thrown when creation of a commit fails
  */
case class UnableToCreateCommit(msg: String) extends SQLError {
  override def toString: String = s"Unable to create a commit: $msg"
}

/**
  * Thrown when creation of a view fails
  */
case class UnableToCreateView(msg: String) extends SQLError {
  override def toString: String = s"Unable to create a View: $msg"
}

/**
  * Catches an unknown exception
  */
case class UnknownSQLException(e: Throwable) extends SQLError {
  override def toString: String = "CAUGHT UNKNOWN SQL EXCEPTION: " + e.toString + "\n" + e.getStackTrace.mkString("\n")  +
    (if (e.getCause != null)"\n" + e.getCause.toString else "")
}