package impl.sql

import core.backend.interfaces.{DBBackend, DBInstance, DatabaseAddress}
import core.containers.ConstrainedFuture
import core.error.E
import core.schema.SchemaDescription

import scala.concurrent.ExecutionContext

object SQLDB extends DBBackend {
  override def open(
                     address: DatabaseAddress,
                     schema: SchemaDescription
                   )(implicit e: ExecutionContext): ConstrainedFuture[E, DBInstance] = ???
}