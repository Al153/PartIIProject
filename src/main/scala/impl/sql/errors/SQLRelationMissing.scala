package impl.sql.errors

import core.error.E
import core.intermediate.unsafe.ErasedRelationAttributes

case class SQLRelationMissing(r: ErasedRelationAttributes) extends E