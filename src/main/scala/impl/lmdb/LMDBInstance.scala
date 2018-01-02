package impl.lmdb

import core.backend.interfaces.{DBExecutor, DBInstance}
import core.containers.ConstrainedFuture
import core.error.E
import core.intermediate.unsafe.UnsafeFindable
import core.schema.{SchemaDescription, TableName}
import core.view.View
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
  */
class LMDBInstance(val env: Env, schema: SchemaDescription)(implicit val executionContext: ExecutionContext) extends DBInstance {
  implicit val instance: LMDBInstance = this

  val db: Database = env.openDatabase()

  override def executor: DBExecutor = new LMDBExecutor()

  override def setDefaultView(view: View): ConstrainedFuture[E, Unit] = controlTables.defaultView.setDefault(view).asCFuture

  override def getDefaultView: ConstrainedFuture[E, View] = controlTables.defaultView.getDefault().asCFuture

  override def getViews: ConstrainedFuture[E, Set[View]] = LMDBFutureE(controlTables.availableViews.availableViews()).asCFuture

  override def close(): Unit = {
    db.close()
    env.close()

  }


  private[lmdb] def validateView(v: View): LMDBEither[Unit] = controlTables.availableViews.validateView(v)

  val objects: Map[TableName, ObjectRetrievalTable] = schema.objectMap.mapValues(s => new ObjectRetrievalTable(s)(this))
  object controlTables {
    val availableViews = new AvailableViewsTable()
    val commitsCounter = new CommitsCounter()
    val defaultView = new DefaultViewTable()
    val relations = new ObjectRelations()
    val reverseRelations = new ObjectReverseRelations()
    val viewsCounter = new ViewsCounter()
    val viewsTable = new ViewsTable()
    val objectCounter = new ObjectsCounter()
  }

  def lookupPattern(p: UnsafeFindable, commits: Set[Commit]): LMDBEither[Set[ObjId]] =
    if (p.tableName in objects) objects(p.tableName).lookup(p, commits)
    else LMDBMissingTable(p.tableName).left


}
