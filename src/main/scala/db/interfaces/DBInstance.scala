package db.interfaces

/**
  * Created by Al on 29/10/2017.
  */
trait DBInstance {
  def executor: DBExecutor


}
