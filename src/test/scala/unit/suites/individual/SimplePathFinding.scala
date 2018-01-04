package unit.suites.individual

import core.backend.interfaces.{DBInstance, Empty}
import core.backend.using
import core.containers.{Operation, PathImpl}
import core.dsl.Commands.{allShortestPaths, insert, shortestPath}
import core.error.E
import core.relations.CompletedRelation
import core.schema.SchemaObject
import org.junit.Test
import unit.Objects._
import unit.{Knows, Person, assertEqOp, description}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scalaz.{-\/, \/-}

/**
  * Created by Al on 23/12/2017.
  */
trait SimplePathFinding {self: HasBackend =>
  /*
      * A number of tests of the pathfinding algorithms
      *
      * A -> B -> E -> F -> G
      */

  private def setupPath(implicit instance: DBInstance, ec: ExecutionContext, sa: SchemaObject[Person]): Operation[E, Unit] = insert(
    CompletedRelation(Alice, Knows, Bob),
    CompletedRelation(Bob, Knows, Eve),
    CompletedRelation(Eve, Knows, Fred),
    CompletedRelation(Fred, Knows, Georgie)
  )

  @Test
  def SimpleShortestPath(): Unit = {
    // A -> B -> E -> F -> G
    val expectedPath = new PathImpl(Vector(Alice -> Bob, Bob -> Eve, Eve -> Fred, Fred -> Georgie))
    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- setupPath
          res1 <- shortestPath(Alice, Georgie, Knows)
          _ <- assertEqOp(expectedPath.getSteps, res1.get.getSteps, "ShortestPaths")
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
  def SimpleAllShortestPaths(): Unit = {
    val expectedPaths = Set(
      new PathImpl(Vector(Alice -> Bob, Bob -> Eve, Eve -> Fred, Fred -> Georgie)),
      new PathImpl(Vector(Alice -> Bob, Bob -> Eve, Eve -> Fred)),
      new PathImpl(Vector(Alice -> Bob, Bob -> Eve)),
      new PathImpl(Vector(Alice -> Bob))
    )
    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- setupPath
          res1 <- allShortestPaths(Alice, Knows)
          _ <- assertEqOp(expectedPaths.map(_.getSteps).toVector.sorted, res1.map(_.getSteps).toVector.sorted, "ShortestPaths")
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
  def SimpleNoEnd(): Unit = {
    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- setupPath
          res2 <- shortestPath(Alice, Zoe, Knows)
          _ <- assertEqOp(None, res2, "ShortestPath, No end")
        } yield res2
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
  def simpleNoStart(): Unit = {
    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- setupPath
          res1 <- shortestPath(Zoe, Alice, Knows)
          _ <- assertEqOp(None, res1, "ShortestPath, No start")
        } yield res1
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
  def simpleNoStartAllShortestPaths(): Unit = {
    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- setupPath
          res3 <- allShortestPaths(Zoe, Knows)
          _ <- assertEqOp(Set(), res3, "All Shortest paths, no start")
        } yield res3
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
