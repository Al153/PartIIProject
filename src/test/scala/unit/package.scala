import core.user.containers.{ConstrainedFuture, Operation, ReadOperation}
import core.user.dsl.{E, HasRecovery, RelationAttributes}
import core.user.schema._
import core.utils.Logged
import errors.{AssertionFailure, UnknownError}
import org.junit.Assert

import scala.concurrent.ExecutionContext

/**
  * Some simple test core.user.schema for unit tests
  */

package object unit extends Logged {
  case class Person(n: String) { def name: String = n}

  implicit def personSchema = new SchemaObject1[Person, String] {
    override def construct(a1: String): Person = Person(a1)
    override def name: TableName = TableName("People")
    override def toTuple(a: Person): SchemaObject[Person] => DBTuple1[Person, String] = buildDBTuple(a.name)
  }

  case class Car(m: String) { def make: String = m}


  implicit def carSchema = new SchemaObject1[Car, String] {
    override def construct(a1: String): Car = Car(a1)
    override def name: TableName = TableName("Cars")
    override def toTuple(a: Car): SchemaObject[Car] => DBTuple1[Car, String] = buildDBTuple(a.make)
  }

  case class Pet(name: String, age: Int, height: Double, isDog: Boolean)

  implicit def petSchema  = new SchemaObject4[Pet, String, Int, Double, Boolean] {
    override def construct(a1: String, a2: Int, a3: Double, a4: Boolean): Pet = Pet(a1, a2, a3, a4)
    override def name: TableName = TableName("Pets")
    override def toTuple(a: Pet): SchemaObject[Pet] => DBTuple4[Pet, String, Int, Double, Boolean] = buildDBTuple(a.name, a.age, a.height, a.isDog)
  }

  case object Knows extends RelationAttributes[Person, Person]
  case object Owns extends RelationAttributes[Person, Car]
  case object OwnedBy extends RelationAttributes[Pet, Person]


  implicit val description = new SchemaDescription(
    Set(personSchema, carSchema, petSchema),
    Set(Knows, Owns, OwnedBy)
  )

  def assertEqOp[E1, A](expected: A, trial: A, msg: String)(implicit ec: ExecutionContext, R: HasRecovery[E1]): Operation[E1, Unit] = new ReadOperation (
    _ => ConstrainedFuture.point[E1, Unit]{
      Assert.assertEquals(msg, expected, trial)
    }


  )

  def assertEq[E1, A](expected: A, trial: A, msg: String)(implicit ec: ExecutionContext, R: HasRecovery[E1]): ConstrainedFuture[E1, Unit] =
    ConstrainedFuture.point[E1, Unit](
      Assert.assertEquals(msg, expected, trial)
    )


  implicit def PersonOrdering(implicit os: Ordering[String]) = new Ordering[Person] {
    override def compare(x: Person, y: Person): Int = os.compare(x.name, y.name)
  }

  implicit def PetOrdering(implicit os: Ordering[String]) = new Ordering[Pet] {
    override def compare(x: Pet, y: Pet): Int = os.compare(x.name, y.name)
  }

  /**
    * Returns \bot but has any type
    * @param e - error to throw
    * @tparam A - type to shoehorn to
    * @return
    */
  def errorThrowable[A](e: E): A = throw new Throwable(e.toString)
}

