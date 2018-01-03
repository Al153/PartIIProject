package unit

import core.backend.interfaces.DBBackend
import impl.lmdb.LMDB
import unit.suites.FullSuite

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits

/**
  * Created by Al on 02/01/2018.
  */
class LMDBTests extends FullSuite {
  override val backend: DBBackend = LMDB
  implicit override val ec: ExecutionContext = Implicits.global
}