package core.user.interfaces

import core.user.containers.{ConstrainedFuture, Operation}
import core.user.dsl.{DatabaseAddress, E, View}
import core.user.schema.SchemaDescription

import scala.concurrent.ExecutionContext
import scalaz.\/

/**
  * Created by Al on 22/10/2017.
  *
  * A database connection can open
  */
trait DBBackend {
  def open(address: DatabaseAddress, schema: SchemaDescription)
          (implicit e: ExecutionContext): \/[E, DBInstance]
}




