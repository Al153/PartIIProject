package unit.suites


import core.containers.Operation
import core.dsl.Commands._
import core.dsl.NodeSyntax.NodeSyntax1
import core.dsl.RelationalQuery._
import core.error.E
import core.relations.CompletedRelation
import core.backend.interfaces.{DBInstance, Empty}
import core.backend.using
import org.junit.Test
import core.schema.SchemaObject
import unit.Objects._
import unit.{Knows, Person, assertEqOp, description, _}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scalaz.{-\/, \/-}

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

  private def setupPath(implicit instance: DBInstance, ec: ExecutionContext, sa: SchemaObject[Person]): Operation[E, Unit] = insert(Set(
    CompletedRelation(Alice, Knows, Bob), CompletedRelation(Bob, Knows, Charlie),
    CompletedRelation(Charlie, Knows, David), CompletedRelation(David, Knows, Eve),
    CompletedRelation(Eve, Knows, Fred), CompletedRelation(Fred, Knows, Georgie),
    CompletedRelation(Georgie, Knows, Hannah), CompletedRelation(Hannah, Knows, Ian)
  ))

  @Test
  def simpleAtLeast(): Unit = {
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
  def simpleExactly(): Unit = {
    val expectedPairs = Vector[(Person, Person)](
      Alice -> Eve,
      Bob -> Fred,
      Charlie -> Georgie,
      David -> Hannah,
      Eve -> Ian
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
  def simpleFullTransitiveClosure(): Unit = {
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
  def simpleBetween(): Unit = {
    val expected = Vector[Person](Alice, Bob, Charlie, David)

    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- setupPath
          res1 <- find(Alice >> Knows * (0 -> 3))
          res2 <- findDistinct(Alice >> Knows * (0 -> 3))
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
  def simpleUpto(): Unit = {
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
