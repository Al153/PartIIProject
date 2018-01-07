package unit.suites.individual

import core.user.containers.{Operation, PathImpl}
import core.user.dsl.{E, _}
import core.user.interfaces.DBInstance
import core.user.schema.SchemaObject
import org.junit.Test
import unit.Objects._
import unit.{Knows, Person, assertEqOp, description}

import scala.concurrent.ExecutionContext

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
  def SimpleShortestPath(): Unit = runTest { implicit instance =>
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
  def SimpleAllShortestPaths(): Unit = runTest { implicit instance =>
    val expectedPaths = Set(
      new PathImpl(Vector(Alice -> Bob, Bob -> Eve, Eve -> Fred, Fred -> Georgie)),
      new PathImpl(Vector(Alice -> Bob, Bob -> Eve, Eve -> Fred)),
      new PathImpl(Vector(Alice -> Bob, Bob -> Eve)),
      new PathImpl(Vector(Alice -> Bob))
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
  def SimpleNoEnd(): Unit = runTest { implicit instance =>
     using(instance) {
      for {
        _ <- setupPath
        res2 <- shortestPath(Alice, Zoe, Knows)
        _ <- assertEqOp(None, res2, "ShortestPath, No end")
      } yield ()
    }
  }

  @Test
  def simpleNoStart(): Unit = runTest { implicit instance =>
    using(instance) {
      for {
        _ <- setupPath
        res1 <- shortestPath(Zoe, Alice, Knows)
        _ <- assertEqOp(None, res1, "ShortestPath, No start")
      } yield ()
    }
  }

  @Test
  def simpleNoStartAllShortestPaths(): Unit = runTest { implicit instance =>
    using(instance) {
      for {
        _ <- setupPath
        res3 <- allShortestPaths(Zoe, Knows)
        _ <- assertEqOp(Set(), res3, "All Shortest paths, no start")
      } yield ()
    }
  }
}
