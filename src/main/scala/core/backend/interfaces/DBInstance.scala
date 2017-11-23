package core.backend.interfaces

import core.containers.ConstrainedFuture
import core.error.E
import core.view.View

import scalaz.\/

/**
  * Created by Al on 29/10/2017.
  */
trait DBInstance {
  def executor: DBExecutor

  def setDefaultView(view: View): E \/ Unit
  def getDefaultView: E \/ View
  def getViews: ConstrainedFuture[E, Set[View]]
}

