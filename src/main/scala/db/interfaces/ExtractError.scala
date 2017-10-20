package db.interfaces

import core.error.E
import schema.SchemaSummary

/**
  * Created by Al on 20/10/2017.
  */

sealed trait ExtractError extends E
case class LengthMismatch() extends ExtractError
case class SchemaMismatch(expected: SchemaSummary, actual: SchemaSummary) extends ExtractError
