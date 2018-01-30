package unit.suites.individual

import core.user.containers.Operation
import core.user.dsl._
import core.user.interfaces.DBInstance
import core.user.schema.SchemaObject
import org.junit.Test
import unit.Objects._
import unit.{Knows, Person, assertEqOp, description, _}

import scala.concurrent.ExecutionContext
import scala.language.postfixOps

/**
  * Created by Al on 04/11/2017.
  */
trait ComplexRepetition { self: HasBackend =>

  /**
    * Check that paths work
    *
    * Set up a grid:
    *
    *  A -> B -> C
    *  |    |    |
    * \/   \/   \/
    *  D -> E -> F
    *  |    |    |
    * \/   \/   \/
    *  G -> H -> I
    *
    *
    */

  private def setupPath(implicit instance: DBInstance, ec: ExecutionContext, sa: SchemaObject[Person]): Operation[E, Unit] =
    insert(
      CompletedRelation(Alice, Knows, Bob), CompletedRelation(Bob, Knows, Charlie),
      CompletedRelation(Alice, Knows, David), CompletedRelation(David, Knows, Eve),
      CompletedRelation(Bob, Knows, Eve), CompletedRelation(Eve, Knows, Fred),
      CompletedRelation(Charlie, Knows, Fred), CompletedRelation(David, Knows, Georgie),
      CompletedRelation(Georgie, Knows, Hannah), CompletedRelation(Eve, Knows, Hannah),
      CompletedRelation(Hannah, Knows, Ian), CompletedRelation(Fred, Knows, Ian)
    )

  @Test
  def atLeast(): Unit = runTest {implicit instance =>
    val expectedPairs = Set[(Person, Person)](
      Alice -> Hannah,
      Alice -> Fred,
      Bob -> Ian,
      David -> Ian,
      Alice -> Ian
    )

    using(instance) {
      for {
        _ <- setupPath
        res1 <- findPairs(Knows * (3 ++) )
        res2 <- findPairs(Knows * (3 ++))
        _ <- assertEqOp(expectedPairs, res1, "Exactly (all pairs)")
        _ <- assertEqOp(expectedPairs, res2, "Exactly (distinct)")
      } yield ()
    }
  }

  @Test
  def exactly(): Unit = runTest { implicit instance =>
    val expectedPairs = Set[(Person, Person)](Alice -> Ian, Alice -> Ian, Alice -> Ian, Alice -> Ian, Alice -> Ian, Alice -> Ian)
    using(instance) {
      for {
        _ <- setupPath
        res1 <- findPairs(Knows * 4)
        res2 <- findPairs(Knows * 4)
        _ <- assertEqOp(expectedPairs, res1, "Exactly (all pairs)")
         _ <- assertEqOp(expectedPairs, res2, "Exactly (distinct)")
      } yield ()
    }
  }

  /**
    *   Find all reachable from alice
    *
    */

  @Test
  def fullTransitiveClosure(): Unit = runTest { implicit instance =>
    val expected = Vector[Person](Alice, Bob, Charlie, David, Eve, Fred, Georgie, Hannah, Ian)
    using(instance) {
      for {
        _ <- setupPath
        res1 <- find(Alice >> Knows.**)
        res2 <- find(Alice >> Knows.**)
        _ <- assertEqOp(expected.toSet, res1, "Exactly (all pairs)")
        _ <- assertEqOp(expected.toSet, res2, "Exactly (distinct)")
      } yield ()
    }
  }

  @Test
  def between(): Unit = runTest { implicit instance =>
    val expected = Vector[Person](Bob, Charlie, David, Eve, Fred, Georgie, Hannah)
    using(instance) {
      for {
        _ <- setupPath
        res2 <- find(Alice >> Knows * (1 --> 3))
        _ <- assertEqOp(expected.toSet, res2, "Exactly (distinct)")
      } yield ()
    }
  }
}
