package db.common

/**
  * Created by Al on 25/10/2017.
  */
sealed trait Limit {def newLimit: Limit}
case object NoLimit extends Limit {
  override def newLimit = NoLimit
}
case class Number(n: Int) extends Limit {
  override def newLimit = Number(n -1)
}