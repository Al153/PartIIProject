package view

/**
  * Created by Al on 13/10/2017.
  *
  * This is a class that handles all inputs and outputs to a view of the database.
  *
  * This should abstract away things like garbage collection in the database, and compile
  * queries to the underlying SQL
  */
trait View {
  def id: Long
}
