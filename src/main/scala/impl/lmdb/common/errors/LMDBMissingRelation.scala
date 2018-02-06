package impl.lmdb.common.errors

import core.backend.common.MissingRelation

/**
  * Created by Al on 29/12/2017.
  *
  * Thrown when unable to find a relation name
  */
case class LMDBMissingRelation(e: MissingRelation) extends LMDBError {

}
