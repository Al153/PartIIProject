package impl.lmdb.logjoins

import impl.lmdb.common.interfaces.{LMDBExecutor, LMDBInstance}
import impl.lmdb.logjoins.methods.Methods

/**
  * Created by Al on 12/12/2017.
  *
  *  Executor implementation in the Methods trait
  */

class LogExecutor(implicit val instance: LMDBInstance) extends LMDBExecutor with Methods
