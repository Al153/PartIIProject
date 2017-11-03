package db.memory

import java.util.concurrent.atomic.AtomicLong

import core.containers.Operation
import core.error.E
import core.intermediate.unsafe.ErasedRelationAttributes
import db.common.MissingViewError
import db.interfaces.{DBExecutor, DBInstance}
import schema.SchemaDescription
import view.View

import scala.concurrent.ExecutionContext
import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 29/10/2017.
  *
  * Instance that hold a database instance
  */
class MemoryInstance(schema: SchemaDescription) extends DBInstance {
  override lazy val executor: DBExecutor = new InMemoryExecutor(this, schema)

  val relations: Set[ErasedRelationAttributes] = schema.relationMap.values.toSet


  private object Store {
    private var memoryStore: Map[View, MemoryTree] = Map()
    private val viewId: AtomicLong = new AtomicLong(0)

    def get(v: View): E \/ MemoryTree = this.synchronized {
      memoryStore.get(v).fold(\/.left[E, MemoryTree](MissingViewError(v)))(_.right)
    }

    def put(t: MemoryTree): E \/ View = this.synchronized {
      val view = new View{val id: Long = viewId.incrementAndGet()}

      memoryStore += view -> t
      return view.right
    }
  }


  def readOp[A](f: MemoryTree => E \/ A)(implicit ec: ExecutionContext): Operation[E, A] =
    Operation.either(v => for {
      t <- Store.get(v)
      a <- f(t)
    } yield (a, v))(throwable => UnknownMemoryError(throwable))

  def writeOp(f: MemoryTree => E \/ MemoryTree)(implicit ec: ExecutionContext): Operation[E, Unit]  =
    Operation.either(u => for {
      t <- Store.get(u)
      newTree <- f(t)
      v <- Store.put(newTree)
    } yield ((), v))(throwable => UnknownMemoryError(throwable))
}
