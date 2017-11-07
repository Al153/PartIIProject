package impl.sql.tables

import core.backend.common.DBObject
import core.containers.Operation
import core.error.E
import core.intermediate.unsafe.UnsafeFindable
import core.view.View

/**
  * An object that represents the store of objects of a particular type
  *
  * Idea: Construct SQL queries by table. Each query should expose an id column to join on
  */
trait ObjectTable {
  def find(unsafeFindable: UnsafeFindable, view: View)
}