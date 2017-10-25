package db.common

/**
  * Created by Al on 25/10/2017.
  */
sealed trait Limit
case object NoLimit extends Limit
case class Number(n: Int) extends Limit