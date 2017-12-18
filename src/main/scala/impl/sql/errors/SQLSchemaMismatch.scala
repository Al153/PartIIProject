package impl.sql.errors

import impl.sql.SQLColumnName
import impl.sql.schema.{ColumnSpecification, SQLType}

/**
  * Created by Al on 11/12/2017.
  */
sealed trait SQLSchemaMismatch extends SQLError
case class SQLSchemaLengthMismatch() extends SQLSchemaMismatch
case class SQLSchemaColumnMissing(column: ColumnSpecification, expected: Map[SQLColumnName, SQLType]) extends SQLSchemaMismatch
case class SQLSchemaTypeMismatch(column: ColumnSpecification, expectedType: SQLType) extends SQLSchemaMismatch
case class SQLSchemaTypeCheckError(expected: SQLType, actual: String) extends SQLSchemaMismatch
case class SQLSchemaUnexpectedType(s: String) extends SQLSchemaMismatch