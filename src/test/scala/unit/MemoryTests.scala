package unit

import db.interfaces.DBBackend
import db.memory.MemoryDB
import unit.suites._

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits

class MemoryTests extends HasBackend with Duplicates with ConjunctionsAndDisjunctions with ReadWrite with Transitive {
  override val backend: DBBackend = MemoryDB
  implicit override val ec: ExecutionContext = Implicits.global
}