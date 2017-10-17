package view

import core.error.E
import core.containers.{ConstrainedFuture, Operation}

/**
  * Created by Al on 13/10/2017.
  *
  * This is a class that handles all inputs and outputs to a view of the database.
  *
  * This should abstract away things like garbage collection in the database, and compile
  * queries to the underlying SQL
  */
trait View {
  val id: Long
  val dependents: Set[View]
  def execute[A](q: Operation[E, A]): ConstrainedFuture[E, (A, View)] = q.runView(this)
}
