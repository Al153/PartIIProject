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
trait ComplexRepetition[E1 <: E] { self: HasBackend[E1] =>

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

  private def setupPath(implicit instance: DBInstance[E1], ec: ExecutionContext, sa: SchemaObject[Person]): Operation[E1, Unit] =
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
        res1 <- findPairs(Knows * (3 ++))
        res2 <- findPairs(Knows * (3 ++))
        _ <- assertEqOp(expectedPairs, res1, "Exactly (all pairs)")
        _ <- assertEqOp(expectedPairs, res2, "Exactly (distinct)")
      } yield ()
    }
  }

  @Test
  def exactly(): Unit = runTest { implicit instance =>
    val expected5 = Set[(Person, Person)]()
    val expected4 = Set[(Person, Person)](Alice -> Ian)
    val expected3 = Set(Alice -> Fred, Alice -> Hannah, Bob -> Ian, David -> Ian)
    val expected2 = Set(
      Alice -> Charlie, Alice -> Eve,
      Alice -> Georgie, Bob -> Fred,
      Bob -> Hannah, David -> Fred,
      David -> Hannah, Charlie -> Ian,
      Eve -> Ian, Georgie -> Ian)
    val expected1 = Set(
      Alice -> Bob, Bob -> Charlie,
      Alice -> David, David -> Eve,
      Bob -> Eve, Eve -> Fred, Charlie -> Fred,
      David -> Georgie, Georgie -> Hannah,
      Eve -> Hannah, Hannah -> Ian, Fred -> Ian
    )
    val expected0 = Set(
      Alice -> Alice, Bob -> Bob, Charlie -> Charlie,
      David -> David, Eve -> Eve, Fred -> Fred,
      Georgie -> Georgie, Hannah -> Hannah, Ian -> Ian
    )
    using(instance) {
      for {
        _ <- setupPath
        res0 <- findPairs(Knows * 0)
        res1 <- findPairs(Knows * 1)
        res2 <- findPairs(Knows * 2)
        res3 <- findPairs(Knows * 3)
        res4 <- findPairs(Knows * 4)
        res5 <- findPairs(Knows * 5)
        _ <- assertEqOp(expected0, res0, "Exactly (0)")
        _ <- assertEqOp(expected1, res1, "Exactly (1)")
        _ <- assertEqOp(expected2, res2, "Exactly (2)")
        _ <- assertEqOp(expected3, res3, "Exactly (3)")
        _ <- assertEqOp(expected4, res4, "Exactly (4)")
        _ <- assertEqOp(expected5, res5, "Exactly (5)")
      } yield ()
    }
  }

  @Test
  def exactlyLeft(): Unit = runTest { implicit instance =>
    val expected5 = Set[Person]()
    val expected4 = Set(Ian)
    val expected3 = Set(Fred, Hannah)
    val expected2 = Set(
      Charlie, Eve,
      Georgie)
    val expected1 = Set(Bob, David)
    val expected0 = Set(Alice)
    using(instance) {
      for {
        _ <- setupPath
        res0 <- find(Alice >> Knows * 0)
        res1 <- find(Alice >> Knows * 1)
        res2 <- find(Alice >> Knows * 2)
        res3 <- find(Alice >> Knows * 3)
        res4 <- find(Alice >> Knows * 4)
        res5 <- find(Alice >> Knows * 5)
        _ <- assertEqOp(expected0, res0, "ExactlyLeft (0)")
        _ <- assertEqOp(expected1, res1, "ExactlyLeft (1)")
        _ <- assertEqOp(expected2, res2, "ExactlyLeft (2)")
        _ <- assertEqOp(expected3, res3, "ExactlyLeft (3)")
        _ <- assertEqOp(expected4, res4, "ExactlyLeft (4)")
        _ <- assertEqOp(expected5, res5, "ExactlyLeft (5)")
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
