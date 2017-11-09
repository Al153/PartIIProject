package impl.sql.tables

import core.intermediate.unsafe.UnsafeFindable
import core.view.View

/**
  * An object that represents the store of objects of a particular type
  *
  * Idea: Construct SQL queries by table. Each query should expose an id column to join on
  */
trait ObjectTable {
  def getRightObject = ??? // apply to a query to get the rightmost object in a query
  def getLeftObject = ??? // apply to a query to get the leftmost object in a query
}