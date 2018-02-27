package unit

import core.user.dsl.HasRecovery
import core.user.interfaces.DBBackend
import impl.memory.MemoryDB
import impl.memory.errors.{MemoryError, MemoryRecovery}
import unit.suites._

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits

class MemoryTests extends FullSuite[MemoryError] {
  override val backend: DBBackend[MemoryError] = MemoryDB
  implicit override val ec: ExecutionContext = Implicits.global

  override implicit def R: HasRecovery[MemoryError] = MemoryRecovery
}