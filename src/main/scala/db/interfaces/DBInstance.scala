package db.interfaces

import core.error.E
import view.View

import scalaz.\/

/**
  * Created by Al on 29/10/2017.
  */
trait DBInstance {
  def executor: DBExecutor

  def setDefaultView(view: View): E \/ Unit
  def getDefaultView: E \/ View
  def getViews: Set[View]
}

