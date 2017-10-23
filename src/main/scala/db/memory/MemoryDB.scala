package db.memory

import core.containers.ConstrainedFuture
import core.error.E
import db.interfaces.{DBBackend, DBInstance, DatabaseAddress}
import schema.SchemaDescription

/**
  * Created by Al on 22/10/2017.
  */
class MemoryDB extends DBBackend {
  val executor = new InMemoryExecutor

  override def open(address: DatabaseAddress, schema: SchemaDescription): ConstrainedFuture[E, DBInstance] = {

  }
}
