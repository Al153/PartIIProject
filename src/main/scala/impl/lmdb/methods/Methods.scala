package impl.lmdb.methods

import impl.lmdb.LMDBInstance

/**
  * Created by Al on 29/12/2017.
  *
  * A trait that implements the methods needed to instantiate a [[core.user.interfaces.DBExecutor]]
  */
trait Methods extends FindAll
  with FindAllPairs
  with Write
  with PathFinding
  with VectorImpl
  with SetImpl {
    implicit val instance: LMDBInstance
}
