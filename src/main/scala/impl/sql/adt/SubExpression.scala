package impl.sql.adt

/**
  * Created by Al on 22/12/2017.
  */
sealed trait SubExpression
case class SimpleSubExpr(q: Query) extends SubExpression
case class Recursive(q: Query) extends SubExpression