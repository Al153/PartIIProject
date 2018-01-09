package impl.memory.errors

import core.user.dsl.View

/**
  * Created by Al on 22/10/2017.
  *
  * Error when there is a missing view
  */
case class MissingViewError(v: View) extends MemoryError {

}
