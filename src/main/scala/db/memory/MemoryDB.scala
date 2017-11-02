package db.memory

import core.containers.ConstrainedFuture
import core.error.E
import db.interfaces.{DBBackend, DBInstance, DatabaseAddress}
import schema.SchemaDescription

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 22/10/2017.
  */
object MemoryDB extends DBBackend {
  override def open(
                     address: DatabaseAddress,
                     schema: SchemaDescription
                   )(implicit e: ExecutionContext): ConstrainedFuture[E, DBInstance] =
    ConstrainedFuture.point[E, DBInstance] {
    new MemoryInstance(schema) // for now just
  }(e => core.error.UnknownError(e))
}
