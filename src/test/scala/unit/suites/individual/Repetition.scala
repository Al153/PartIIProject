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
trait Repetition { self: HasBackend =>

  /**
    * Check that paths work, simple example
    *
    * Set up a grid:
    *
    *  A -> B -> C ->
    *  D -> E -> F ->
    *  G -> H -> I
    *
    *
    */

  private def setupPath(implicit instance: DBInstance, ec: ExecutionContext, sa: SchemaObject[Person]): Operation[E, Unit] = insert(
    CompletedRelation(Alice, Knows, Bob), CompletedRelation(Bob, Knows, Charlie),
    CompletedRelation(Charlie, Knows, David), CompletedRelation(David, Knows, Eve),
    CompletedRelation(Eve, Knows, Fred), CompletedRelation(Fred, Knows, Georgie),
    CompletedRelation(Georgie, Knows, Hannah), CompletedRelation(Hannah, Knows, Ian)
  )

  @Test
  def simpleAtLeast(): Unit = runTest { implicit instance =>
    val expectedPairs = Vector[(Person, Person)](
      Alice -> David,
      Alice -> Eve,
      Alice -> Fred,
      Alice -> Georgie,
      Alice -> Hannah,
      Alice -> Ian,

      Bob -> Eve,
      Bob -> Fred,
      Bob -> Georgie,
      Bob -> Hannah,
      Bob -> Ian,


      Charlie -> Fred,
      Charlie -> Georgie,
      Charlie -> Hannah,
      Charlie -> Ian,

      David -> Georgie,
      David -> Hannah,
      David -> Ian,


      Eve -> Hannah,
      Eve -> Ian,

      Fred -> Ian
    )

    using(instance) {
      for {
        _ <- setupPath
        res1 <- findPairs(Knows * (3 ++))
        res2 <- findPairsDistinct(Knows * (3 ++ ))
        _ <- assertEqOp(expectedPairs.sorted, res1.sorted, "Simple Atleast (all pairs)")
        _ <- assertEqOp(expectedPairs.toSet, res2, "Simple Atleast (distinct)")
      } yield ()
    }
  }

  @Test
  def simpleExactly(): Unit = runTest { implicit instance =>
    val expectedPairs = Vector[(Person, Person)](
      Alice -> Eve,
      Bob -> Fred,
      Charlie -> Georgie,
      David -> Hannah,
      Eve -> Ian
    )
    using(instance) {
      for {
        _ <- setupPath
        res1 <- findPairs(Knows * 4)
        res2 <- findPairsDistinct(Knows * 4)
        _ <- assertEqOp(expectedPairs.sorted, res1.sorted, "Exactly (all pairs)")
        _ <- assertEqOp(expectedPairs.toSet, res2, "Exactly (distinct)")
      } yield ()
    }
  }

  /**
    *   Find all reachable from alice
    *
    */

  @Test
  def simpleFullTransitiveClosure(): Unit = runTest { implicit instance =>
    val expected = Vector[Person](Alice, Bob, Charlie, David, Eve, Fred, Georgie, Hannah, Ian)
    using(instance) {
      for {
        _ <- setupPath
        res1 <- find(Alice >> Knows.**)
        res2 <- findDistinct(Alice >> Knows.**)
        _ <- assertEqOp(expected.toSet, res1.toSet, "Exactly (all pairs)")
        _ <- assertEqOp(expected.toSet, res2, "Exactly (distinct)")
      } yield ()
    }
  }

  @Test
  def simpleBetween(): Unit = runTest { implicit instance =>
    val expected = Vector[Person](Alice, Bob, Charlie, David)

    using(instance) {
      for {
        _ <- setupPath
        res1 <- find(Alice >> Knows * (0 --> 3))
        res2 <- findDistinct(Alice >> Knows * (0 --> 3))
        _ <- assertEqOp(expected.toSet, res1.toSet, "Exactly (all pairs)")
        _ <- assertEqOp(expected.toSet, res2, "Exactly (distinct)")
      } yield ()
    }
  }

  @Test
  def simpleUpto(): Unit = runTest { implicit instance =>
    val expected = Vector[Person](Alice, Bob)

    using(instance) {
      for {
        _ <- setupPath
        res1 <- find(Alice >> Knows.?)
        res2 <- findDistinct(Alice >> Knows.?)
        _ <- assertEqOp(expected.toSet, res1.toSet, "Up to (all pairs)")
        _ <- assertEqOp(expected.toSet, res2, "Up to (distinct)")
      } yield ()
    }
  }
}
