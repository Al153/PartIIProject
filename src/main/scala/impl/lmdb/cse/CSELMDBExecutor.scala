package impl.lmdb.cse

import impl.lmdb.common.interfaces.{LMDBExecutor, LMDBInstance}
import impl.lmdb.fast.methods.Methods

/**
  * Created by Al on 12/12/2017.
  *
  *  Executor implementation in the Methods trait
  */

class CSELMDBExecutor(implicit val instance: LMDBInstance) extends LMDBExecutor with Methods
