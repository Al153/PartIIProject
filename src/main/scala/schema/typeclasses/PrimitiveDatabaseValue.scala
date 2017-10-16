package schema.typeclasses

/**
  * Created by Al on 02/10/2017.
  * primitive values that can be stored in the database
  */
sealed trait PrimitiveDatabaseValue[T]

object PrimitiveDatabaseValue {
  implicit object PrimitiveString extends PrimitiveDatabaseValue[String]
  implicit object PrimitiveInt extends PrimitiveDatabaseValue[Int]
  implicit object PrimitiveLong extends PrimitiveDatabaseValue[Long]
  implicit object PrimitiveDouble extends PrimitiveDatabaseValue[Double]
  implicit object PrimitiveBoolean extends PrimitiveDatabaseValue[Boolean]
}
