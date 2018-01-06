package impl.memory.errors

import core.user.schema.TableName

/**
  * Created by Al on 04/01/2018.
  */
case class MemoryMissingTableName(t: TableName) extends MemoryError

