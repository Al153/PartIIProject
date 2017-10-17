package schema.typeclasses

/**
  * Created by Al on 02/10/2017.
  * primitive values that can be stored in the database
  */
sealed trait DatabaseValue[T]

object DatabaseValue {
  implicit case object PrimitiveString extends DatabaseValue[String]
  implicit case object PrimitiveInt extends DatabaseValue[Int]
  implicit case object PrimitiveLong extends DatabaseValue[Long]
  implicit case object PrimitiveDouble extends DatabaseValue[Double]
  implicit case object PrimitiveBoolean extends DatabaseValue[Boolean]
}
