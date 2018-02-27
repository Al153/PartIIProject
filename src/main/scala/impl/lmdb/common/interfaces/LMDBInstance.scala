package impl.lmdb.common.interfaces

import java.nio.ByteBuffer

import core.backend.intermediate.unsafe.ErasedFindable
import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, View}
import core.user.interfaces.DBInstance
import core.user.schema.{SchemaDescription, TableName}
import impl.lmdb.common._
import impl.lmdb.common.access.{Commit, ObjId}
import impl.lmdb.common.errors.{LMDBError, LMDBMissingTable}
import impl.lmdb.common.tables.impl._
import org.lmdbjava.Env
import core.utils._

import scalaz.Scalaz._
import scala.concurrent.ExecutionContext

abstract class LMDBInstance(val env: Env[ByteBuffer], val schema: SchemaDescription)(implicit val executionContext: ExecutionContext) extends DBInstance[LMDBError] {
  // makes passing to other methods easier
  private [lmdb] implicit val instance: LMDBInstance = this

  /**
    * @return internal executor
    */
  override def executor: LMDBExecutor

  /**
    * Delegate responsibility
    */
  override def setDefaultView(view: View): LMDBFuture[Unit] = controlTables.defaultView.setDefault(view)


  /**
    * Delegate responsibility
    */
  override def getDefaultView: LMDBFuture[View] = controlTables.defaultView.getDefault()


  /**
    * Delegate responsibility
    */
  override def getViews: LMDBFuture[Set[View]] = LMDBFutureE(controlTables.availableViews.availableViews())




  /**
    * Helper method to validate view
    */
  private[lmdb] def validateView(v: View): LMDBEither[Unit] = controlTables.availableViews.validateView(v)

  /**
    * Contains an [[ObjectRetrievalTable]] for each of the tables in the schema
    */
  private [lmdb] val objects: Map[TableName, ObjectRetrievalTable] = schema.objectMap.mapValues(s => new ObjectRetrievalTable(s)(this))

  /**
    * Control tables are the ones used to run the instance
    *
    * The general model of the LMDB instance is to store all relations in `relations` and `reverseRelations`
    * To execute queries: the start points' [[ObjId]] s are looked up via the index tables in the relevant
    * [[ObjectRetrievalTable]], the query is then executed on object ids to get a coollection of ids.
    *
    * The Ids are then looked up in thre retrieval tables to give the fully constructed results
    *
    */
  private [lmdb] object controlTables {
    val availableViews = new AvailableViewsTable()
    val commitsCounter = new CommitsCounter()
    val defaultView = new DefaultViewTable()
    val relations = new ObjectRelations()
    val reverseRelations = new ObjectReverseRelations()
    val viewsCounter = new ViewsCounter()
    val viewsTable = new ViewsTable()
    val objectCounter = new ObjectsCounter()
  }

  def initialise(): LMDBEither[Unit]

  /**
    * Lookup a pattern, helper method
    */
  private [lmdb] def lookupPattern(p: ErasedFindable, commits: List[Commit]): LMDBEither[Set[ObjId]] =
    if (p.tableName in objects) objects(p.tableName).lookup(p, commits)
    else LMDBMissingTable(p.tableName).left
}

