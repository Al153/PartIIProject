package unit

import core.backend.interfaces.DBBackend
import impl.memory.MemoryDB
import unit.suites._

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits

class MemoryTests extends FullSuite {
  override val backend: DBBackend = MemoryDB
  implicit override val ec: ExecutionContext = Implicits.global
}