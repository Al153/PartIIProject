package impl.memory

import core.user.interfaces.{DBBackend, DBInstance}
import core.user.containers.ConstrainedFuture
import core.user.dsl.{DatabaseAddress, E}
import core.user.schema.SchemaDescription
import impl.memory.errors.MemoryError

import scala.concurrent.ExecutionContext
import scalaz._
import Scalaz._

/**
  * Created by Al on 22/10/2017.
  *
  *  A simple in memory implementation of a backend
  */
object MemoryDB extends DBBackend[MemoryError] {
  /**
    * Always opens a new DB
    * @param address - Address to open
    * @param schema - Schema to open with
    * @return a [[DBInstance]]
    */
  override def open(
                     address: DatabaseAddress,
                     schema: SchemaDescription
                   )(implicit e: ExecutionContext): MemoryEither[DBInstance[MemoryError]] = new MemoryInstance(schema).right // for now just

}
