package impl.lmdb.errors

import core.backend.common.{DBObject, ExtractError}

/**
  * Created by Al on 02/01/2018.
  */
sealed trait LMDBExtractError extends LMDBError

case class UnexpectedStreamLength(expectedLength: Int, stream: Seq[Byte]) extends LMDBExtractError
case class BooleanExtractError(stream: Seq[Byte]) extends LMDBExtractError
case class UnrecognisedDBHeader(header: Byte) extends LMDBExtractError
case class UnmarshallingError(e: ExtractError) extends LMDBExtractError
