package unit.suites

import core.backend.interfaces.{DBInstance, Empty}
import core.backend.using
import core.containers.Operation
import core.dsl.Commands.{find, findDistinct, findPairs, findPairsDistinct, _}
import core.dsl.NodeSyntax._
import core.dsl.RelationalQuery._
import core.dsl.Repetition._
import core.error.E
import core.relations.CompletedRelation
import core.schema.SchemaObject
import org.junit.Test
import unit.Objects._
import unit.{Knows, Person, assertEqOp, description, _}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scala.language.postfixOps
import scalaz.{-\/, \/-}

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

  private def setupPath(implicit instance: DBInstance, ec: ExecutionContext, sa: SchemaObject[Person]): Operation[E, Unit] = insert(Set(
    CompletedRelation(Alice, Knows, Bob), CompletedRelation(Bob, Knows, Charlie),
    CompletedRelation(Alice, Knows, David), CompletedRelation(David, Knows, Eve),
    CompletedRelation(Bob, Knows, Eve), CompletedRelation(Eve, Knows, Fred),
    CompletedRelation(Charlie, Knows, Fred), CompletedRelation(David, Knows, Georgie),
    CompletedRelation(Georgie, Knows, Hannah), CompletedRelation(Eve, Knows, Hannah),
    CompletedRelation(Hannah, Knows, Ian), CompletedRelation(Fred, Knows, Ian)
  ))

  @Test
  def atLeast(): Unit = {
    val expectedPairs = Set[(Person, Person)](
      Alice -> Hannah,
      Alice -> Fred,
      Bob -> Ian,
      David -> Ian,
      Alice -> Ian
    )
    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- setupPath
          res1 <- findPairs(Knows * (3 ++) )
          res2 <- findPairsDistinct(Knows * (3 ++))
          _ <- assertEqOp(expectedPairs, res1.toSet, "Exactly (all pairs)")
          _ <- assertEqOp(expectedPairs, res2, "Exactly (distinct)")
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
  def exactly(): Unit = {
    val expectedPairs = Vector[(Person, Person)](Alice -> Ian, Alice -> Ian, Alice -> Ian, Alice -> Ian, Alice -> Ian, Alice -> Ian)
    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- setupPath
          res1 <- findPairs(Knows * 4)
          res2 <- findPairsDistinct(Knows * 4)
          _ <- assertEqOp(expectedPairs, res1, "Exactly (all pairs)")
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
  def fullTransitiveClosure(): Unit = {
    val expected = Vector[Person](Alice, Bob, Charlie, David, Eve, Fred, Georgie, Hannah, Ian)
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
  def between(): Unit = {
    val expected = Vector[Person](Bob, Charlie, David, Eve, Fred, Georgie, Hannah)

    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- setupPath
          res1 <- find(Alice >> Knows * (1 --> 3))
          res2 <- findDistinct(Alice >> Knows * (1 --> 3))
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


}
