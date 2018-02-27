package unit.suites.individual

import core.user.interfaces.DBInstance
import core.user.dsl.{E, CompletedRelation, _}
import core.user.containers.{Operation, PathImpl}
import core.user.schema.SchemaObject
import org.junit.Test
import unit.Objects._
import unit.{Knows, Person, assertEqOp, description}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scalaz.{-\/, \/-}

trait ComplexPathFinding[E1 <: E] {
  self: HasBackend[E1] =>


  /*
    * A number of tests of the pathfinding algorithms
    *
    * A -> B -> E -> F -> G
    *      |    ^    ^
    *     \/    |    |
    *      C -> D -> H
    *      ^    |
    *      |   \/
    *      J <- I
    */

  private def setupPath(implicit instance: DBInstance[E1], ec: ExecutionContext, sa: SchemaObject[Person]): Operation[E1, Unit] = insert(
    CompletedRelation(Alice, Knows, Bob),
    CompletedRelation(Bob, Knows, Eve),
    CompletedRelation(Bob, Knows, Charlie),
    CompletedRelation(Charlie, Knows, David),
    CompletedRelation(David, Knows, Eve),
    CompletedRelation(Eve, Knows, Fred),
    CompletedRelation(Fred, Knows, Georgie),
    CompletedRelation(David, Knows, Hannah),
    CompletedRelation(Hannah, Knows, Fred),
    CompletedRelation(David, Knows, Ian),
    CompletedRelation(Ian, Knows, Jane),
    CompletedRelation(Jane, Knows, Charlie)
  )

  @Test
  def ShortestPath(): Unit = runTest { implicit instance =>
    // A -> B -> E -> F -> G
    val expectedPath = new PathImpl(Vector(Alice -> Bob, Bob -> Eve, Eve -> Fred, Fred -> Georgie))
    using(instance) {
      for {
        _ <- setupPath
        res1 <- shortestPath(Alice, Georgie, Knows)
        _ <- assertEqOp(expectedPath.getSteps, res1.get.getSteps, "ShortestPaths")
      } yield ()
    }
  }

  @Test
  def AllShortestPaths(): Unit = runTest { implicit instance =>
    val expectedPaths = Set(
      new PathImpl(Vector(Alice -> Bob, Bob -> Eve, Eve -> Fred, Fred -> Georgie)),
      new PathImpl(Vector(Alice -> Bob, Bob -> Eve, Eve -> Fred)),
      new PathImpl(Vector(Alice -> Bob, Bob -> Eve)),
      new PathImpl(Vector(Alice -> Bob)),
      new PathImpl(Vector(Alice -> Bob, Bob -> Charlie)),
      new PathImpl(Vector(Alice -> Bob, Bob -> Charlie, Charlie -> David)),
      new PathImpl(Vector(Alice -> Bob, Bob -> Charlie, Charlie -> David, David -> Hannah)),
      new PathImpl(Vector(Alice -> Bob, Bob -> Charlie, Charlie -> David, David -> Ian)),
      new PathImpl(Vector(Alice -> Bob, Bob -> Charlie, Charlie -> David, David -> Hannah)),
      new PathImpl(Vector(Alice -> Bob, Bob -> Charlie, Charlie -> David, David -> Ian, Ian -> Jane))

    )

    using(instance) {
      for {
        _ <- setupPath
        res1 <- allShortestPaths(Alice, Knows)
        _ <- assertEqOp(expectedPaths.map(_.getSteps).toVector.sorted, res1.map(_.getSteps).toVector.sorted, "ShortestPaths")
      } yield ()
    }
  }

  @Test
  def noEnd(): Unit = runTest { implicit instance =>
    using(instance) {
      for {
        _ <- setupPath
        res1 <- shortestPath(Zoe, Alice, Knows)
        res2 <- shortestPath(Alice, Zoe, Knows)
        res3 <- allShortestPaths(Zoe, Knows)
        _ <- assertEqOp(None, res1, "ShortestPath, No start")
        _ <- assertEqOp(None, res2, "ShortestPath, No end")
        _ <- assertEqOp(Set(), res3, "All Shortest paths, no start")
      } yield ()
    }
  }
}