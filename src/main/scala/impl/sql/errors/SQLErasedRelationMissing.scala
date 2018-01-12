package impl.sql.errors

import core.backend.intermediate.unsafe.ErasedRelationAttributes

/**
  * Occurs when the user tries to use an unknown relation
  */
case class SQLErasedRelationMissing(r: ErasedRelationAttributes) extends SQLError