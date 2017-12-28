package impl.lmdb

import core.backend.interfaces.{DBExecutor, DBInstance}
import core.containers.ConstrainedFuture
import core.error.E
import core.view.View
import org.fusesource.lmdbjni.Env

/**
  * Created by Al on 12/12/2017.
  */
class LMDBInstance(env: Env) extends DBInstance {


  override def executor: DBExecutor = ???

  override def setDefaultView(view: View): ConstrainedFuture[E, Unit] = ???

  override def getDefaultView: ConstrainedFuture[E, View] = ???

  override def getViews: ConstrainedFuture[E, Set[View]] = ???
}
