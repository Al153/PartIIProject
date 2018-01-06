package unit

import core.user.interfaces.DBBackend
import impl.memory.MemoryDB
import unit.suites._

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits

class MemoryTests extends FullSuite {
  override val backend: DBBackend = MemoryDB
  implicit override val ec: ExecutionContext = Implicits.global
}