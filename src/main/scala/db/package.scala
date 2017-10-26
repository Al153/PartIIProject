import db.interfaces.DBInstance
import view.View

/**
  * Created by Al on 07/10/2017.
  */
package object db {
  def using[A](db: DBInstance)(action: View => A) = ???
}
