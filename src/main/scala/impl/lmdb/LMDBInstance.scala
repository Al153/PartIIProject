package impl.lmdb

import core.user.interfaces.{DBExecutor, DBInstance}
import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, View}
import core.backend.intermediate.unsafe.ErasedFindable
import core.user.schema.{SchemaDescription, TableName}
import impl.lmdb.access.{Commit, ObjId}

import scalaz._
import Scalaz._
import impl.lmdb.tables.impl._
import org.fusesource.lmdbjni.{Database, Env}
import core.utils._
import impl.lmdb.errors.LMDBMissingTable

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 12/12/2017.
  *
  * [[DBInstance]] implementation
  */
final class LMDBInstance(val env: Env, val schema: SchemaDescription)(implicit val executionContext: ExecutionContext) extends DBInstance {
  // makes passing to other methods easier
  private [lmdb] implicit val instance: LMDBInstance = this

  // underlying database connection
  private [lmdb] val db: Database = env.openDatabase()

  /**
    * @return internal executor
    */
  override def executor: DBExecutor = new LMDBExecutor()

  /**
    * Delegate responsibility
    */
  override def setDefaultView(view: View): ConstrainedFuture[E, Unit] = controlTables.defaultView.setDefault(view).asCFuture


  /**
    * Delegate responsibility
    */
  override def getDefaultView: ConstrainedFuture[E, View] = controlTables.defaultView.getDefault().asCFuture


  /**
    * Delegate responsibility
    */
  override def getViews: ConstrainedFuture[E, Set[View]] = LMDBFutureE(controlTables.availableViews.availableViews()).asCFuture

  /**
    * Close DB and env
    */
  override def close(): Unit = {
    db.close()
    env.close()

  }


  /**
    * Helper method to validate view
    * @param v
    * @return
    */
  private[lmdb] def validateView(v: View): LMDBEither[Unit] = controlTables.availableViews.validateView(v)

  /**
    * Contains an [[ObjectRetrievalTable]] for each of the tables in the schema
    */
  private [lmdb] val objects: Map[TableName, ObjectRetrievalTable] = schema.objectMap.mapValues(s => new ObjectRetrievalTable(s)(this))

  /**
    * Control tables are the ones used to run the instance
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

  /**
    * Lookup a pattern, helper method
    */
  private [lmdb] def lookupPattern(p: ErasedFindable, commits: Set[Commit]): LMDBEither[Set[ObjId]] =
    if (p.tableName in objects) objects(p.tableName).lookup(p, commits)
    else LMDBMissingTable(p.tableName).left


}
