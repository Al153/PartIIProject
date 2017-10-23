package db.common

import core.error.E
import schema.{SchemaComponent, TableName}

/**
  * Created by Al on 20/10/2017.
  */

sealed trait ExtractError extends E
case class LengthMismatch() extends ExtractError
case class SchemaMismatch(expected: SchemaComponent, actual: SchemaComponent) extends ExtractError
case class SchemaNumberMismatch(expected: DBObject, actual: DBObject) extends ExtractError
case class MissingTableName(t: TableName) extends ExtractError
