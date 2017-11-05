package db.interfaces

import core.containers.{ConstrainedFuture, Operation}
import core.error.E
import schema.SchemaDescription
import view.View

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 22/10/2017.
  *
  * A database connection can open
  */
trait DBBackend {
  def open(address: DatabaseAddress, schema: SchemaDescription)(implicit e: ExecutionContext): ConstrainedFuture[E, DBInstance]
}




