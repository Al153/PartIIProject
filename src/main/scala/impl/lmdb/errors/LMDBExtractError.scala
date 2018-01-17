package impl.lmdb.errors

import core.backend.common.ExtractError

/**
  * Created by Al on 02/01/2018.
  *
  * Errors from extracting values from the LMDB
  */
sealed trait LMDBExtractError extends LMDBError

/**
  * Expected: 255, or 0. This is thrown when a byte does not match these values
  * @param b
  */
case class BooleanExtractError(b: Byte) extends LMDBExtractError
case class UnrecognisedDBHeader(header: Byte) extends LMDBExtractError
case class UnmarshallingError(e: ExtractError) extends LMDBExtractError
