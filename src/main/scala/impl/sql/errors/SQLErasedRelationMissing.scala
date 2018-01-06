package impl.sql.errors

import core.backend.intermediate.unsafe.ErasedRelationAttributes

case class SQLErasedRelationMissing(r: ErasedRelationAttributes) extends SQLError