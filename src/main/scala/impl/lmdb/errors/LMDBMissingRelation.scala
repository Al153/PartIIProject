package impl.lmdb.errors

import core.backend.common.MissingRelation

/**
  * Created by Al on 29/12/2017.
  */
case class LMDBMissingRelation(e: MissingRelation) extends LMDBError {

}
