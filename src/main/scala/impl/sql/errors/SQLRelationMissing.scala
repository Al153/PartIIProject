package impl.sql.errors

import core.intermediate.unsafe.ErasedRelationAttributes

case class SQLRelationMissing(r: ErasedRelationAttributes) extends SQLError