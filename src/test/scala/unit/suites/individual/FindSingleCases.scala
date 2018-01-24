package unit.suites.individual

import core.user.containers.Operation
import core.user.dsl._
import core.user.interfaces.DBInstance
import core.user.schema.SchemaObject
import org.junit.Test
import unit.Objects._
import unit._
import unit.{Knows, OwnedBy, Person}

import scala.concurrent.ExecutionContext
import scalaz._, Scalaz._

trait FindSingleCases { self: HasBackend =>
  private def setupPath(implicit instance: DBInstance, ec: ExecutionContext, sa: SchemaObject[Person]): Operation[E, Unit] =
    for {
      _ <-  insert(
        CompletedRelation(fido, OwnedBy, Alice),
        CompletedRelation(rover, OwnedBy, Alice),
        CompletedRelation(polly, OwnedBy, Bob),
        CompletedRelation(leo, OwnedBy, Charlie),
        CompletedRelation(buster, OwnedBy, David),

        CompletedRelation(gus, OwnedBy, Eve),
        CompletedRelation(fin, OwnedBy, Fred),
        CompletedRelation(tufty, OwnedBy, Fred),
        CompletedRelation(tilly, OwnedBy, Georgie),
        CompletedRelation(pippa, OwnedBy, Georgie),

        CompletedRelation(luna, OwnedBy, Hannah),
        CompletedRelation(nelson, OwnedBy, Hannah),
        CompletedRelation(lucy, OwnedBy, Ian),
        CompletedRelation(jasper, OwnedBy, Jane)
      )

      _ <- insert(
        CompletedRelation(Bob, Knows, Alice),
        CompletedRelation(Fred, Knows, Georgie),
        CompletedRelation(Hannah, Knows, Ian)
      )
    } yield ()

  @Test
  def Union(): Unit = runTest { implicit  instance =>
    val expected = Set(Alice, David, Eve, Fred, Georgie, Hannah, Ian, Jane)

    val dog = petSchema.pattern(None, None, None, true.some)
    val is4 = petSchema.pattern(None, 4.some, None, None)

    using(instance) {
      for {
        _ <- setupPath
        dogOwnersAndFourYearOldOwners <- find((dog >> OwnedBy) | (is4 >> OwnedBy))
        _ <- assertEqOp(expected, dogOwnersAndFourYearOldOwners, "Failed Single Union")
      } yield ()
    }
  }


  @Test
  def Intersection(): Unit = runTest { implicit  instance =>
    val expected = Set(Hannah, Ian)

    val dog = petSchema.pattern(None, None, None, true.some)
    val is3 = petSchema.pattern(None, 3.some, None, None)

    using(instance) {
      for {
        _ <- setupPath
        threeYearOldDogOwners <- find((dog >> OwnedBy) & (is3 >> OwnedBy))
        _ <- assertEqOp(expected, threeYearOldDogOwners, "Failed Single Intersection")
      } yield ()
    }
  }
}