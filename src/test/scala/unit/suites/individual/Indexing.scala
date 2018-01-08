package unit.suites.individual

import core.user.containers.Operation
import core.user.dsl.{CompletedRelation, E, insert, using, _}
import core.user.interfaces.DBInstance
import core.user.schema.SchemaObject
import org.junit.Test
import unit.Objects._
import unit.{Knows, OwnedBy, Person, _}

import scala.concurrent.ExecutionContext
import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 08/01/2018.
  */
trait Indexing { self: HasBackend =>

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
  def SingleIndex(): Unit = runTest { implicit instance =>
    val expectedDog = Vector(fido, rover, buster, pippa, nelson, lucy, jasper)
    val expectedNotDog = Vector(polly, leo, gus, fin, tufty, tilly, luna)
    val expectedFido = Vector(Alice)
    val expectedAgeSix = Vector(tufty, luna)
    val expectedHeightSix = Vector(tufty, tilly)
    using(instance) {
      for {
        _ <- setupPath

        rDog <- find(petSchema.pattern(None, None, None, true.some))
        rNotDog <- find(petSchema.pattern(None, None, None, false.some))
        rFido <- find(petSchema.pattern("Fido".some, None, None, None) >> OwnedBy)
        rAgeSix <- find(petSchema.pattern(None, 6.some, None, None))
        rHeightSix <- find(petSchema.pattern(None, None, 6.0.some, None))

        _ <- assertEqOp(expectedDog.sorted, rDog.sorted, "Single Index, Dog")
        _ <- assertEqOp(expectedNotDog.sorted, rNotDog.sorted, "Single Index, NotDog")
        _ <- assertEqOp(expectedFido.sorted, rFido.sorted, "Single Index, Fido")
        _ <- assertEqOp(expectedAgeSix.sorted, rAgeSix.sorted, "Single Index, AgeSix")
        _ <- assertEqOp(expectedHeightSix.sorted, rHeightSix.sorted, "Single Index, HeightSix")
      } yield ()
    }


  }

  @Test
  def LeftAnd(): Unit = runTest { implicit instance =>
    val expected = Vector(fido -> Alice, rover -> Alice, buster -> David, pippa -> Georgie, nelson -> Hannah, lucy -> Ian, jasper -> Jane)
    using(instance) {
        for {
          _ <- setupPath

          dogOwnership <- findPairs(petSchema.pattern(None, None, None, true.some) -->> OwnedBy)


        _ <- assertEqOp(expected.sorted, dogOwnership.sorted, "LeftAnd")
      } yield ()
    }
  }

  @Test
  def RightAnd(): Unit = runTest { implicit instance =>

    val expected = Vector(Bob -> fido, Bob -> rover, Fred -> pippa, Hannah -> lucy)
    using(instance) {
      for {
        _ <- setupPath

        knowsOwner <- findPairs((Knows --><-- OwnedBy) -->> petSchema.pattern(None, None, None, true.some) )


        _ <- assertEqOp(expected.sorted, knowsOwner.sorted, "rightAnd")
      } yield ()
    }
  }


}
