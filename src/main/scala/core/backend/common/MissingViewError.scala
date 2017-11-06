package core.backend.common

import core.error.E
import core.view.View

/**
  * Created by Al on 22/10/2017.
  */
case class MissingViewError(v: View) extends E {

}
