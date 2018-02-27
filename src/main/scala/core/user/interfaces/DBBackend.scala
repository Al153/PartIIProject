package core.user.interfaces

import core.user.containers.{ConstrainedFuture, Operation}
import core.user.dsl.{DatabaseAddress, E, View}
import core.user.schema.SchemaDescription
import core.utils.Logged

import scala.concurrent.ExecutionContext
import scalaz.\/

/**
  * Created by Al on 22/10/2017.
  *
  * A database connection can open an [[DBInstance]]
  */
trait DBBackend[E <: core.user.dsl.E] extends Logged {
  /**
    * Open an Instance
    * @param address - Address to open
    * @param schema - Schema to open with
    * @return a [[DBInstance]]
    */
  def open(address: DatabaseAddress, schema: SchemaDescription)
          (implicit e: ExecutionContext): \/[E, DBInstance[E]]
}




