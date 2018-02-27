package impl.memory

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

import core.user.interfaces.{DBExecutor, DBInstance}
import core.user.containers.{ConstrainedFuture, Operation, ReadOperation, WriteOperation}
import core.user.dsl.{E, View}
import core.backend.intermediate.unsafe.ErasedRelationAttributes
import core.user.schema.SchemaDescription
import core.utils._
import impl.memory.errors.{MemoryError, MissingViewError}

import scala.collection.JavaConverters._
import scala.collection.concurrent
import scala.concurrent.ExecutionContext
import scalaz.Scalaz._

/**
  * Created by Al on 29/10/2017.
  *
  * Instance implementation
  */
class MemoryInstance(
                      val schema: SchemaDescription
                    )(
  implicit val executionContext: ExecutionContext
) extends DBInstance[MemoryError] {
  override lazy val executor: DBExecutor[MemoryError] = new InMemoryExecutor(this)

  /**
    * initial default tree
    */
  val defaultTree: MemoryTree = schema.objects.map(o => o.name -> MemoryTable(o)).toMap

  /**
    * stores the mutable state for the instance
    */
  private object Store {
    private var defaultView: View = View(0)
    /**
      * Stores View => Tree
      */
    private val memoryStore: concurrent.Map[View, MemoryTree] = new ConcurrentHashMap[View, MemoryTree]().asScala

    /**
      * View counter - mutable
      */
    private val viewId: AtomicLong = new AtomicLong(1)

    // Setup the default view
    memoryStore(defaultView) = defaultTree

    /**
      * Pick a tree according to the view
      */
    def get(v: View): MemoryEither[MemoryTree] = this.synchronized {
      memoryStore.getOrError(v, MissingViewError(v))
    }

    /**
      * Insert a tree and get its view
      */
    def put(t: MemoryTree): MemoryEither[View] = this.synchronized {
      val view = View(viewId.incrementAndGet())
      memoryStore(view) = t
      return view.right
    }

    /**
      * get default view
      */
    def getDefaultView: View = this.synchronized(defaultView)
    /**
      * set default view
      */
    def setDefaultView(v: View): Unit = this.synchronized(defaultView = v)
    /**
      * get all views
      */
    def getViews: Set[View] = memoryStore.keys.toSet
  }

  /**
    * Wraps a function of [[MemoryTree]] into an [[Operation]]
    */

  def readOp[A](f: MemoryTree => MemoryEither[A]): MemoryOperation[A] =
    new ReadOperation(v => MemoryFutureE(for {
      t <- Store.get(v)
      a <- f(t)
    } yield a))

  /**
    * Wraps a function of [[MemoryTree]] => [[MemoryTree]] into an [[Operation]]
    */

  def writeOp(f: MemoryTree => MemoryEither[MemoryTree]): MemoryOperation[Unit]  =
    new WriteOperation (u => MemoryFutureE(for {
      t <- Store.get(u)
      newTree <- f(t)
      v <- Store.put(newTree)
    } yield v))

  /**
    * Implements trait method
    */
  override def setDefaultView(view: View): MemoryFuture[Unit] = MemoryFutureI(Store.setDefaultView(view))

  /**
    * Implements trait method
    */

  override def getDefaultView: MemoryFuture[View] = MemoryFutureI(Store.getDefaultView)

  /**
    * Implements trait method
    */

  override def getViews: MemoryFuture[Set[View]] = MemoryFuture(Store.getViews)

  /**
    * Implements trait method
    */

  override def close(): Unit = ()
}
