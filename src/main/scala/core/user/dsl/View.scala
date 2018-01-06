package core.user.dsl

/**
  * Created by Al on 13/10/2017.
  *
  * This is a class that handles all inputs and outputs to a core.view of the database.
  *
  * This should abstract away things like garbage collection in the database, and compile
  * queries to the underlying SQL
  */
case class View(id: Long) extends AnyVal
