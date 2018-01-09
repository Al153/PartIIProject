package impl.memory.errors

import core.user.schema.TableName

/**
  * Created by Al on 04/01/2018.
  *
  * Error for when a [[TableName]] is missing from the instance
  */
case class MemoryMissingTableName(t: TableName) extends MemoryError

