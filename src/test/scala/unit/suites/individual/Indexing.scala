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
trait Indexing[E1 <: E] { self: HasBackend[E1] =>

  private def setupPath(implicit instance: DBInstance[E1], ec: ExecutionContext, sa: SchemaObject[Person]): Operation[E1, Unit] =
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
    val expectedDog = Set(fido, rover, buster, pippa, nelson, lucy, jasper)
    val expectedNotDog = Set(polly, leo, gus, fin, tufty, tilly, luna)
    val expectedFido = Set(Alice)
    val expectedAgeSix = Set(tufty, luna)
    val expectedHeightSix = Set(tufty, tilly)
    using(instance) {
      for {
        _ <- setupPath

        rDog <- find(petSchema.pattern(None, None, None, true.some))
        rNotDog <- find(petSchema.pattern(None, None, None, false.some))
        rFido <- find(petSchema.pattern("Fido".some, None, None, None) >> OwnedBy)
        rAgeSix <- find(petSchema.pattern(None, 6.some, None, None))
        rHeightSix <- find(petSchema.pattern(None, None, 6.0.some, None))

        _ <- assertEqOp(expectedDog, rDog, "Single Index, Dog")
        _ <- assertEqOp(expectedNotDog, rNotDog, "Single Index, NotDog")
        _ <- assertEqOp(expectedFido, rFido, "Single Index, Fido")
        _ <- assertEqOp(expectedAgeSix, rAgeSix, "Single Index, AgeSix")
        _ <- assertEqOp(expectedHeightSix, rHeightSix, "Single Index, HeightSix")
      } yield ()
    }
  }

  @Test
  def MultiIndex(): Unit = runTest { implicit instance =>
    val expected = Set(pippa, jasper)
    using(instance) {
      for {
        _ <- setupPath
        r <- find(petSchema.pattern(None, 12.some, None, true.some))
        _ <- assertEqOp(expected, r, "Single Index, Dog")
      } yield ()
    }
  }





}
