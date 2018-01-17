package impl.lmdb

import java.nio.ByteBuffer

import core.user.interfaces.{DBExecutor, DBInstance}
import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, View}
import core.backend.intermediate.unsafe.ErasedFindable
import core.user.schema.{SchemaDescription, TableName}
import impl.lmdb.access.{Commit, ObjId}

import scalaz._
import Scalaz._
import impl.lmdb.tables.impl._
import org.lmdbjava.Env
// import org.fusesource.lmdbjni.{Database, Env}
import core.utils._
import impl.lmdb.errors.LMDBMissingTable

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 12/12/2017.
  *
  * [[DBInstance]] implementation
  */
final class LMDBInstance(val env: Env[ByteBuffer], val schema: SchemaDescription, isNew: Boolean)(implicit val executionContext: ExecutionContext) extends DBInstance {
  // makes passing to other methods easier
  private [lmdb] implicit val instance: LMDBInstance = this

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

  def initialise(): LMDBEither[Unit] = if (isNew) for {
    _ <- controlTables.availableViews.initialise()
  _ = println("Initialising")
    _ <- controlTables.commitsCounter.initialise()
    _ <- controlTables.defaultView.initialise()
    _ <- controlTables.relations.initialise()
    _ <- controlTables.reverseRelations.initialise()
    _ <- controlTables.viewsCounter.initialise()
    _ <- controlTables.viewsTable.initialise()
    _ <- controlTables.objectCounter.initialise()
    _ <- EitherOps.sequence(objects.map {case (_, table) => table.initialise()})
  } yield () else LMDBEither(())

  /**
    * Lookup a pattern, helper method
    */
  private [lmdb] def lookupPattern(p: ErasedFindable, commits: Set[Commit]): LMDBEither[Set[ObjId]] =
    if (p.tableName in objects) objects(p.tableName).lookup(p, commits)
    else LMDBMissingTable(p.tableName).left


}
