package impl.lmdb.common.errors

import core.backend.common.{ExtractError, MissingRelation}
import core.backend.intermediate.unsafe.UnsafeFindSingle
import core.user.dsl.{E, View}
import core.user.schema.TableName
import impl.lmdb.common.access.ObjId

/**
  * Created by Al on 13/12/2017.
  *
  * Trait hierarchy
  */
sealed trait LMDBError extends E {}

/**
  * Created by Al on 28/12/2017.
  *
  *  Catches unknown exceptions
  */
case class CaughtLMDBException(e: Throwable) extends LMDBError {
  override def toString: String = "CAUGHT UNKNOWN LMDB EXCEPTION: " + e.toString + "\n" + e.getStackTrace.mkString("\n")
}

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
/**
  * Created by Al on 01/01/2018.
  *
  * Error thrown when a value we want to insert does not appear in the pregenerated index
  * (ie something has gone very wrong)
  */
case class LMDBMissingInsert[A](a: A, lookup: Map[A, ObjId]) extends LMDBError {

}


/**
  * Created by Al on 29/12/2017.
  *
  * Thrown when unable to find a relation name
  */
case class LMDBMissingRelation(e: MissingRelation) extends LMDBError {

}


/**
  * Created by Al on 02/01/2018.
  *
  * Created when a view doesn't exist
  */
case class InvalidView(v: View) extends LMDBError

/**
  * Created by Al on 29/12/2017.
  *
  * Thrown when an unknown TableName is looked up
  */
case class LMDBMissingTable(table: TableName) extends LMDBError {

}

/**
  * Occurs when a cached UnsafeFindSingle is missing
  * @param q
  */
case class MissingCachedQuery(q: UnsafeFindSingle) extends LMDBError

/**
  * Created by Al on 01/01/2018.
  *
  * Thrown when an ObjId is not in the pregenerated index During an extract. (ie something has gone very wrong)
  */
case class MissingIndex[A](id: ObjId, index: Map[ObjId, A]) extends LMDBError {

}

/**
  * Dummy error value for when a transactional get and set fails
  */
object NoResult extends LMDBError {

}
