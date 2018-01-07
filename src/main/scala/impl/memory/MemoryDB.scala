package impl.memory

import core.user.interfaces.{DBBackend, DBInstance}
import core.user.containers.ConstrainedFuture
import core.user.dsl.{DatabaseAddress, E}
import core.user.schema.SchemaDescription

import scala.concurrent.ExecutionContext
import scalaz._, Scalaz._

/**
  * Created by Al on 22/10/2017.
  *
  *  A simple in memory implementation of a backend
  */
object MemoryDB extends DBBackend {
  override def open(
                     address: DatabaseAddress,
                     schema: SchemaDescription
                   )(implicit e: ExecutionContext): \/[E, DBInstance] = new MemoryInstance(schema).right // for now just

}
