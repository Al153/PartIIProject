package impl.lmdb.errors

import core.backend.common.ExtractError

/**
  * Created by Al on 02/01/2018.
  *
  * Errors from extracting values from the LMDB
  */
sealed trait LMDBExtractError extends LMDBError

case class UnexpectedStreamLength(expectedLength: Int, stream: Seq[Byte]) extends LMDBExtractError
case class BooleanExtractError(stream: Seq[Byte]) extends LMDBExtractError
case class UnrecognisedDBHeader(header: Byte) extends LMDBExtractError
case class UnmarshallingError(e: ExtractError) extends LMDBExtractError
