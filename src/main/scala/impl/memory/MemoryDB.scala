package impl.memory

import core.backend.interfaces.{DBBackend, DBInstance, DatabaseAddress}
import core.containers.ConstrainedFuture
import core.error.E
import core.schema.SchemaDescription

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 22/10/2017.
  *
  *  A simple in memory implementation of a backend
  */
object MemoryDB extends DBBackend {
  override def open(
                     address: DatabaseAddress,
                     schema: SchemaDescription
                   )(implicit e: ExecutionContext): ConstrainedFuture[E, DBInstance] =
    ConstrainedFuture.immediatePoint[E, DBInstance] {
      new MemoryInstance(schema) // for now just
    }
}
