package impl.memory

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

import core.backend.common.MissingViewError
import core.backend.interfaces.{DBExecutor, DBInstance}
import core.containers.Operation
import core.error.E
import core.intermediate.unsafe.ErasedRelationAttributes
import core.schema.SchemaDescription
import core.view.View
import core.utils._

import scala.collection.JavaConverters._
import scala.collection.concurrent
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
  val defaultTree: MemoryTree = schema.erasedObjects.map(o => o.name -> MemoryTable(o)).toMap

  private object Store { // stores the mutable state
    private var defaultView: View = new View{val id: Long = 0}
    private val memoryStore: concurrent.Map[View, MemoryTree] = new ConcurrentHashMap[View, MemoryTree]().asScala
    private val viewId: AtomicLong = new AtomicLong(1)

    memoryStore(defaultView) = defaultTree

    def get(v: View): E \/ MemoryTree = this.synchronized {
      memoryStore.getOrError(v, MissingViewError(v))
    }

    def put(t: MemoryTree): E \/ View = this.synchronized {
      val view = new View{val id: Long = viewId.incrementAndGet()}
      memoryStore(view) = t
      return view.right
    }

    def getDefaultView: View = this.synchronized(defaultView)
    def setDefaultView(v: View): Unit = this.synchronized(defaultView = v)
    def getViews: Set[View] = memoryStore.keys.toSet
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

  override def setDefaultView(view: View): E \/ Unit = Store.setDefaultView(view).right

  override def getDefaultView: E \/ View = Store.getDefaultView.right

  override def getViews: Set[View] = Store.getViews
}