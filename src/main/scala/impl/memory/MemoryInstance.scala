package impl.memory

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

import core.backend.common.MissingViewError
import core.backend.interfaces.{DBExecutor, DBInstance}
import core.containers.{ConstrainedFuture, Operation}
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
class MemoryInstance(schema: SchemaDescription)(implicit val executionContext: ExecutionContext) extends DBInstance {
  override lazy val executor: DBExecutor = new InMemoryExecutor(this, schema)

  val relations: Set[ErasedRelationAttributes] = schema.relationMap.values.toSet
  val defaultTree: MemoryTree = schema.erasedObjects.map(o => o.name -> MemoryTable(o)).toMap

  private object Store { // stores the mutable state
    private var defaultView: View = View(0)
    private val memoryStore: concurrent.Map[View, MemoryTree] = new ConcurrentHashMap[View, MemoryTree]().asScala
    private val viewId: AtomicLong = new AtomicLong(1)

    memoryStore(defaultView) = defaultTree

    def get(v: View): E \/ MemoryTree = this.synchronized {
      memoryStore.getOrError(v, MissingViewError(v))
    }

    def put(t: MemoryTree): E \/ View = this.synchronized {
      val view = View(viewId.incrementAndGet())
      memoryStore(view) = t
      return view.right
    }

    def getDefaultView: View = this.synchronized(defaultView)
    def setDefaultView(v: View): Unit = this.synchronized(defaultView = v)
    def getViews: Set[View] = memoryStore.keys.toSet
  }


  def readOp[A](f: MemoryTree => E \/ A): Operation[E, A] =
    Operation.either(v => for {
      t <- Store.get(v)
      a <- f(t)
    } yield (a, v))(throwable => UnknownMemoryError(throwable))

  def writeOp(f: MemoryTree => E \/ MemoryTree): Operation[E, Unit]  =
    Operation.either(u => for {
      t <- Store.get(u)
      newTree <- f(t)
      v <- Store.put(newTree)
    } yield ((), v))(throwable => UnknownMemoryError(throwable))

  override def setDefaultView(view: View): ConstrainedFuture[E, Unit] = ConstrainedFuture.immediatePoint(Store.setDefaultView(view))

  override def getDefaultView: ConstrainedFuture[E, View] = ConstrainedFuture.immediatePoint(Store.getDefaultView)

  override def getViews: ConstrainedFuture[E, Set[View]] = ConstrainedFuture.point[E, Set[View]](Store.getViews)(UnknownMemoryError)

  override def close(): Unit = ()
}
