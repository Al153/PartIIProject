package core.backend.common

import core.schema.{SchemaComponent, TableName}

/**
  * Created by Al on 20/10/2017.
  */

sealed trait ExtractError
case class LengthMismatch(patternLength: Int, indexLength: Int) extends ExtractError
case class SchemaMismatch(expected: SchemaComponent, actual: DBCell) extends ExtractError
case class SchemaNumberMismatch(expected: DBObject, actual: DBObject) extends ExtractError
case class MissingTableName(t: TableName) extends ExtractError
