import core.containers.{ConstrainedFuture, Operation, ReadOperation}
import core.error.E
import core.relations.RelationAttributes
import org.junit.Assert
import core.schema._

import scala.concurrent.ExecutionContext

/**
  * Some simple test core.schema for unit tests
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
      case e => core.error.UnknownError(e)
    }
  )

  def assertEq[A](expected: A, trial: A, msg: String)(implicit ec: ExecutionContext): ConstrainedFuture[E, Unit] =
    ConstrainedFuture.point(Assert.assertEquals(expected, trial)) {
      case e: AssertionError => AssertionFailure(e, msg)
      case e => core.error.UnknownError(e)
    }

  case class AssertionFailure(e: Throwable, msg: String) extends E {
    override def toString: String = s"AssertionFailure:\n$msg\n${e.getMessage}"
  }

  implicit def PersonOrdering(implicit os: Ordering[String]) = new Ordering[Person] {
    override def compare(x: Person, y: Person): Int = os.compare(x.name, y.name)
  }
}

