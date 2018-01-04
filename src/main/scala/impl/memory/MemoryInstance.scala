package impl.memory

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

import core.backend.interfaces.{DBExecutor, DBInstance}
import core.containers.{ConstrainedFuture, Operation, ReadOperation, WriteOperation}
import core.error.E
import core.intermediate.unsafe.ErasedRelationAttributes
import core.schema.SchemaDescription
import core.utils._
import core.view.View
import impl.memory.errors.MissingViewError

import scala.collection.JavaConverters._
import scala.collection.concurrent
import scala.concurrent.ExecutionContext
import scalaz.Scalaz._

/**
  * Created by Al on 29/10/2017.
  *
  * Instance that hold a database instance
  */
class MemoryInstance(schema: SchemaDescription)(implicit val executionContext: ExecutionContext) extends DBInstance {
  override lazy val executor: DBExecutor = new InMemoryExecutor(this, schema)

  val relations: Set[ErasedRelationAttributes] = schema.relationMap.values.toSet
  val defaultTree: MemoryTree = schema.erasedObjects.map(o => o.name -> MemoryTable(o)).toMap

  private object Store { // stores the mutable state
    private var defaultView: View = View(0)
    private val memoryStore: concurrent.Map[View, MemoryTree] = new ConcurrentHashMap[View, MemoryTree]().asScala
    private val viewId: AtomicLong = new AtomicLong(1)

    memoryStore(defaultView) = defaultTree

    def get(v: View): MemoryEither[MemoryTree] = this.synchronized {
      memoryStore.getOrError(v, MissingViewError(v))
    }

    def put(t: MemoryTree): MemoryEither[View] = this.synchronized {
      val view = View(viewId.incrementAndGet())
      memoryStore(view) = t
      return view.right
    }

    def getDefaultView: View = this.synchronized(defaultView)
    def setDefaultView(v: View): Unit = this.synchronized(defaultView = v)
    def getViews: Set[View] = memoryStore.keys.toSet
  }


  def readOp[A](f: MemoryTree => MemoryEither[A]): Operation[E, A] =
    new ReadOperation(v => MemoryFutureE(for {
      t <- Store.get(v)
      a <- f(t)
    } yield a).asCFuture)

  def writeOp(f: MemoryTree => MemoryEither[MemoryTree]): Operation[E, Unit]  =
    new WriteOperation (u => MemoryFutureE(for {
      t <- Store.get(u)
      newTree <- f(t)
      v <- Store.put(newTree)
    } yield v).asCFuture)

  override def setDefaultView(view: View): ConstrainedFuture[E, Unit] = ConstrainedFuture.immediatePoint(Store.setDefaultView(view))

  override def getDefaultView: ConstrainedFuture[E, View] = MemoryFutureI(Store.getDefaultView).asCFuture

  override def getViews: ConstrainedFuture[E, Set[View]] = MemoryFuture(Store.getViews).asCFuture

  override def close(): Unit = ()
}
