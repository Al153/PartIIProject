package impl.sql.errors

import core.intermediate.unsafe.ErasedRelationAttributes

case class SQLErasedRelationMissing(r: ErasedRelationAttributes) extends SQLError