package db.common

import core.error.E
import view.View

/**
  * Created by Al on 22/10/2017.
  */
case class MissingViewError(v: View) extends E {

}
