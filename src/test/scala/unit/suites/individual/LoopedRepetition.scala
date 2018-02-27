package unit.suites.individual

import core.user.containers.Operation
import core.user.dsl._
import core.user.interfaces.DBInstance
import core.user.schema.SchemaObject
import org.junit.Test
import unit.Objects._
import unit.{Knows, Person, assertEqOp, description}

import scala.concurrent.ExecutionContext
import scala.language.postfixOps

trait LoopedRepetition[E1 <: E] { self: HasBackend[E1] =>
  /**
    * Check that paths work, on a cyclic graph
    *
    * A -> B -> C -> D -> A
    *
    */

  private def setupPath(implicit instance: DBInstance[E1], ec: ExecutionContext, sa: SchemaObject[Person]): Operation[E1, Unit] = insert(
    CompletedRelation(Alice, Knows, Bob), CompletedRelation(Bob, Knows, Charlie),
    CompletedRelation(Charlie, Knows, David), CompletedRelation(David, Knows, Alice)
  )

  @Test
  def loopedAtLeast(): Unit = runTest { implicit instance =>
    val expectedPairs = Set(
      Alice -> Alice,
      Alice -> Bob,
      Alice -> Charlie,
      Alice -> David,

      Bob -> Alice,
      Bob -> Bob,
      Bob -> Charlie,
      Bob -> David,

      Charlie -> Alice,
      Charlie -> Bob,
      Charlie -> Charlie,
      Charlie -> David,

      David -> Alice,
      David -> Bob,
      David -> Charlie,
      David -> David

    )
    using(instance) {
      for {
        _ <- setupPath
        res1 <- findPairs(Knows * (3 ++))
        res2 <- findPairs(Knows * (3 ++))
        _ <- assertEqOp(expectedPairs, res1, "Simple Atleast (all pairs)")
        _ <- assertEqOp(expectedPairs, res2, "Simple Atleast (distinct)")
      } yield ()
    }
  }

  @Test
  def loopedExactly(): Unit = runTest { implicit instance =>
    val expectedPairs = Set[(Person, Person)](
      Alice -> Alice,
      Bob -> Bob,
      Charlie -> Charlie,
      David -> David
    )

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
  def loopedFullTransitiveClosure(): Unit = runTest {implicit instance =>
    val expected = Set[Person](Alice, Bob, Charlie, David)
    using(instance) {
      for {
        _ <- setupPath
        res1 <- find(Alice >> Knows.**)
        res2 <- find(Alice >> Knows.**)
        _ <- assertEqOp(expected, res1, "Exactly (all pairs)")
        _ <- assertEqOp(expected, res2, "Exactly (distinct)")
      } yield ()
    }
  }

  @Test
  def loopedBetween(): Unit = runTest {implicit instance =>
    val expected = Set[Person](Charlie, David)

    using(instance) {
      for {
        _ <- setupPath
        res1 <- find(Alice >> Knows * (2 --> 3))
        res2 <- find(Alice >> Knows * (2 --> 3))
        _ <- assertEqOp(expected, res1, "Exactly (all pairs)")
        _ <- assertEqOp(expected, res2, "Exactly (distinct)")
      } yield ()
    }
  }

  @Test
  def loopedUpto(): Unit = runTest {implicit instance =>
    val expected = Set[Person](Alice, Bob)

    using(instance) {
      for {
        _ <- setupPath
        res1 <- find(Alice >> Knows.?)
        res2 <- find(Alice >> Knows.?)
        _ <- assertEqOp(expected, res1, "Up to (all pairs)")
        _ <- assertEqOp(expected, res2, "Up to (distinct)")
      } yield ()
    }
  }
}