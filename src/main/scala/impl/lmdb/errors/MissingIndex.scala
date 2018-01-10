package impl.lmdb.errors

import impl.lmdb.access.ObjId

/**
  * Created by Al on 01/01/2018.
  *
  * Thrown when an ObjId is not in the pregenerated index (ie something has gone very wrong)
  */
case class MissingIndex[A](id: ObjId, index: Map[ObjId, A]) extends LMDBError {

}
