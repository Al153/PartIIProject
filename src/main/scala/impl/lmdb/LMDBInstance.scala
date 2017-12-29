package impl.lmdb

import core.backend.interfaces.{DBExecutor, DBInstance}
import core.containers.ConstrainedFuture
import core.error.E
import core.schema.{SchemaDescription, TableName}
import core.view.View
import impl.lmdb.tables.impl._
import org.fusesource.lmdbjni.Env

/**
  * Created by Al on 12/12/2017.
  */
class LMDBInstance(env: Env, schema: SchemaDescription) extends DBInstance {


  override def executor: DBExecutor = ???

  override def setDefaultView(view: View): ConstrainedFuture[E, Unit] = ???

  override def getDefaultView: ConstrainedFuture[E, View] = ???

  override def getViews: ConstrainedFuture[E, Set[View]] = ???

  val objects: Map[TableName, ObjectRetrievalTable] = schema.objectMap.mapValues(s => new ObjectRetrievalTable(s)(this))
  object controlTables {
    val availableViews = new AvailableViewsTable()(this)
    val commitsCounter = new CommitsCounter()(this)
    val defaultView = new DefaultViewTable()(this)
    val relations = new ObjectRelations()(this)
    val reverseRelations = new ObjectReverseRelations()(this)
    val viewsCounter = new ViewsCounter()(this)
    val viewsTable = new ViewsTable()(this)
    val objectCounter = new ObjectsCounter()(this)
  }
}
