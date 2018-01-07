import core.user.containers.{ConstrainedFuture, Operation, ReadOperation}
import core.user.dsl.{E, RelationAttributes}
import core.user.schema._
import errors.{AssertionFailure, UnknownError}
import org.junit.Assert

import scala.concurrent.ExecutionContext

/**
  * Some simple test core.user.schema for unit tests
  */

package object unit {
  case class Person(n: String) { def name: String = n}
  case class Car(m: String) { def make: String = m}


  implicit def personSchema = new SchemaObject1[Person, String] {
    override def construct(a1: String): Person = Person(a1)
    override def tableName: TableName = TableName("People")
    override def toTuple(a: Person): DBTuple1[Person, String] = DBTuple1(tableName, a.name)
  }

  implicit def carSchema = new SchemaObject1[Car, String] {
    override def construct(a1: String): Car = Car(a1)
    override def tableName: TableName = TableName("Cars")
    override def toTuple(a: Car): DBTuple1[Car, String] = DBTuple1(tableName, a.make)
  }

  case object Knows extends RelationAttributes[Person, Person]
  case object Owns extends RelationAttributes[Person, Car]


  implicit val description = new SchemaDescription(
    Set(personSchema, carSchema),
    Set(Knows, Owns)
  )

  def assertEqOp[A](expected: A, trial: A, msg: String)(implicit ec: ExecutionContext): Operation[E, Unit] = new ReadOperation (
    _ => ConstrainedFuture.point(Assert.assertEquals(expected, trial)) {
      case e: AssertionError => AssertionFailure(e, msg)
      case e => UnknownError(e)
    }
  )

  def assertEq[A](expected: A, trial: A, msg: String)(implicit ec: ExecutionContext): ConstrainedFuture[E, Unit] =
    ConstrainedFuture.point(Assert.assertEquals(expected, trial)) {
      case e: AssertionError => AssertionFailure(e, msg)
      case e => UnknownError(e)
    }


  implicit def PersonOrdering(implicit os: Ordering[String]) = new Ordering[Person] {
    override def compare(x: Person, y: Person): Int = os.compare(x.name, y.name)
  }

  def errorThrowable[A](e: E): A = throw new Throwable {
    override def toString: String = e.toString
  }
}

