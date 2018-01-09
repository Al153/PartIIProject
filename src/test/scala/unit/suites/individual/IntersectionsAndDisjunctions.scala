package unit.suites.individual

import core.user.containers.Operation
import core.user.dsl._
import core.user.interfaces.DBInstance
import core.user.schema.SchemaObject
import org.junit.Test
import unit.Objects._
import unit.{Knows, OwnedBy, Owns, Person, assertEqOp, description, petSchema}
import scalaz._, Scalaz._

import scala.concurrent.ExecutionContext

trait IntersectionsAndDisjunctions { self: HasBackend =>

  /**
    * Test that Intersections and disjunctions work
    */


  @Test
  def IntersectionsAndDisjunctions(): Unit = runTest { implicit instance =>
    val expectedUnion = Vector(
      Alice -> Charlie,
      Alice -> Charlie,
      Alice -> Bob,
      Alice -> Fred,
      Bob -> Eve,
      Alice -> Eve,
      Eve -> Alice,
      Charlie -> Eve,
      Charlie -> Alice,
      Eve -> Charlie,
      Fred -> Bob,
      Bob -> Fred
    )
    val expectedIntersection = Vector(Alice -> Charlie)

    using(instance) {
      for {
        _ <- insert(
          CompletedRelation(Alice, Knows, Bob),
          CompletedRelation(Alice, Knows, Charlie),
          CompletedRelation(Alice, Knows, Fred),
          CompletedRelation(Bob, Knows, Eve)
        )

        _ <- insert(
          CompletedRelation(Alice, Owns, Ford),
          CompletedRelation(Charlie, Owns, Ford),
          CompletedRelation(Eve, Owns, Ford),
          CompletedRelation(Fred, Owns, Mercedes),
          CompletedRelation(Bob, Owns, Mercedes)
        )

        res4 <- findPairs(Knows | (Owns --><-- Owns))
        res3 <- findPairs(Knows & (Owns --><-- Owns))
        res2 <- findPairsDistinct(Knows | (Owns --><-- Owns))
        res1 <- findPairsDistinct((Owns --><-- Owns) & Knows)

        _ <- assertEqOp(expectedUnion.sorted, res4.sorted, "union Failure (All)")
        _ <- assertEqOp(expectedIntersection.sorted, res3.sorted, "intersection Failure (All)")
        _ <- assertEqOp(expectedIntersection.toSet, res1, "Intersection failure (Distinct)")
        _ <- assertEqOp(expectedUnion.toSet, res2, "Union failure, (Distinct)")
      } yield ()
    }
  }

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

}