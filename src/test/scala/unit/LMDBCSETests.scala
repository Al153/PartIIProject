package unit

import core.user.interfaces.DBBackend
import impl.lmdb.cse.LMDB
import unit.suites.FullSuite

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits

/**
  * Created by Al on 02/01/2018.
  */
class LMDBCSETests extends FullSuite {
  override val backend: DBBackend = LMDB
  implicit override val ec: ExecutionContext = Implicits.global
}