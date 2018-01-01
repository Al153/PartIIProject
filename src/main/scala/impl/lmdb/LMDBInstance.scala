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
import org.fusesource.lmdbjni.Env
import core.utils._
import impl.lmdb.errors.LMDBMissingTable

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 12/12/2017.
  */
class LMDBInstance(val env: Env, schema: SchemaDescription)(implicit executionContext: ExecutionContext) extends DBInstance {
  implicit val ec = executionContext

  override def executor: DBExecutor = ???

  override def setDefaultView(view: View): ConstrainedFuture[E, Unit] = ???

  override def getDefaultView: ConstrainedFuture[E, View] = ???

  override def getViews: ConstrainedFuture[E, Set[View]] = ???

  implicit val instance = this

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
