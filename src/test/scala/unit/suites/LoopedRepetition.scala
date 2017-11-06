package unit.suites

import core.containers.Operation
import core.dsl.Commands._
import core.dsl.NodeSyntax._
import core.error.E
import core.relations.CompletedRelation
import core.backend.interfaces.{DBInstance, Empty}
import core.backend.using
import org.junit.Test
import core.schema.SchemaObject
import unit.Objects._
import unit.{Knows, Person, assertEqOp, description}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scalaz.{-\/, \/-}

trait LoopedRepetition { self: HasBackend =>
  /**
    * Check that paths work, on a cyclic graph
    *
    * A -> B -> C -> D -> A
    *
    */

  private def setupPath(implicit instance: DBInstance, ec: ExecutionContext, sa: SchemaObject[Person]): Operation[E, Unit] = insert(Set(
    CompletedRelation(Alice, Knows, Bob), CompletedRelation(Bob, Knows, Charlie),
    CompletedRelation(Charlie, Knows, David), CompletedRelation(David, Knows, Alice)
  ))

  @Test
  def loopedAtLeast(): Unit = {
    val expectedPairs = Vector[(Person, Person)](
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
    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- setupPath
          res1 <- findPairs(Knows *+ 3)
          res2 <- findPairsDistinct(Knows *+ 3)
          _ <- assertEqOp(expectedPairs.sorted, res1.sorted, "Simple Atleast (all pairs)")
          _ <- assertEqOp(expectedPairs.toSet, res2, "Simple Atleast (distinct)")
        } yield ()
    }

    Await.result(
      op.run , 2.seconds
    ) match {
      case \/-(_) => ()
      case -\/(e) => throw new Throwable {
        override def toString: String = e.toString
      }
    }
  }

  @Test
  def loopedExactly(): Unit = {
    val expectedPairs = Vector[(Person, Person)](
      Alice -> Alice,
      Bob -> Bob,
      Charlie -> Charlie,
      David -> David
    )
    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- setupPath
          res1 <- findPairs(Knows * 4)
          res2 <- findPairsDistinct(Knows * 4)
          _ <- assertEqOp(expectedPairs.sorted, res1.sorted, "Exactly (all pairs)")
          _ <- assertEqOp(expectedPairs.toSet, res2, "Exactly (distinct)")
        } yield ()
    }

    Await.result(
      op.run , 2.seconds
    ) match {
      case \/-(_) => ()
      case -\/(e) => throw new Throwable {
        override def toString: String = e.toString
      }
    }
  }

  /**
    *   Find all reachable from alice
    *
    */

  @Test
  def loopedFullTransitiveClosure(): Unit = {
    val expected = Vector[Person](Alice, Bob, Charlie, David)
    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- setupPath
          res1 <- find(Alice >> Knows.**)
          res2 <- findDistinct(Alice >> Knows.**)
          _ <- assertEqOp(expected.toSet, res1.toSet, "Exactly (all pairs)")
          _ <- assertEqOp(expected.toSet, res2, "Exactly (distinct)")
        } yield ()
    }

    Await.result(
      op.run , 2.seconds
    ) match {
      case \/-(_) => ()
      case -\/(e) => throw new Throwable {
        override def toString: String = e.toString
      }
    }
  }

  @Test
  def loopedBetween(): Unit = {
    val expected = Vector[Person](Charlie, David)

    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- setupPath
          res1 <- find(Alice >> Knows * (2 -> 3))
          res2 <- findDistinct(Alice >> Knows * (2 -> 3))
          _ <- assertEqOp(expected.toSet, res1.toSet, "Exactly (all pairs)")
          _ <- assertEqOp(expected.toSet, res2, "Exactly (distinct)")
        } yield ()
    }

    Await.result(
      op.run , 2.seconds
    ) match {
      case \/-(_) => ()
      case -\/(e) => throw new Throwable {
        override def toString: String = e.toString
      }
    }
  }

  @Test
  def loopedUpto(): Unit = {
    val expected = Vector[Person](Alice, Bob)

    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- setupPath
          res1 <- find(Alice >> Knows.?)
          res2 <- findDistinct(Alice >> Knows.?)
          _ <- assertEqOp(expected.toSet, res1.toSet, "Up to (all pairs)")
          _ <- assertEqOp(expected.toSet, res2, "Up to (distinct)")
        } yield ()
    }

    Await.result(
      op.run , 2.seconds
    ) match {
      case \/-(_) => ()
      case -\/(e) => throw new Throwable {
        override def toString: String = e.toString
      }
    }
  }
}