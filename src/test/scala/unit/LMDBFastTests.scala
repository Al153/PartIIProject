package unit

import core.user.dsl.HasRecovery
import core.user.interfaces.DBBackend
import impl.lmdb.common.errors.{LMDBError, LMDBRecover}
import impl.lmdb.fast.LMDB
import unit.suites.FullSuite

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits

/**
  * Created by Al on 02/01/2018.
  */
class LMDBFastTests extends FullSuite[LMDBError] {
  override implicit def R: HasRecovery[LMDBError] = LMDBRecover
  override val backend: DBBackend[LMDBError] = LMDB
  implicit override val ec: ExecutionContext = Implicits.global
}