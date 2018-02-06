package impl.lmdb.original

import core.user.interfaces.DBExecutor
import impl.lmdb.common.interfaces.{LMDBExecutor, LMDBInstance}
import impl.lmdb.original.methods.Methods

/**
  * Created by Al on 12/12/2017.
  *
  *  Executor implementation in the Methods trait
  */

class OriginalLMDBExecutor(implicit val instance: LMDBInstance) extends LMDBExecutor with Methods
