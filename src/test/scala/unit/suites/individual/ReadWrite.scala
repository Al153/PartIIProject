package unit.suites.individual

import core.backend.interfaces.Empty
import core.backend.using
import core.dsl.Commands.{find, findPairs, insert}
import core.dsl.NodeSyntax._
import core.relations.CompletedRelation
import core.utils._
import org.junit.Test
import unit.Objects._
import unit.{Knows, Person, assertEqOp, description}

import scala.concurrent.Await
import scala.concurrent.duration._
import scalaz.{-\/, \/-}

trait ReadWrite { self: HasBackend =>
  /**
    * A write followed by a read should result in the written values being read
    */

  @Test
  def WriteAndReadPair(): Unit = {
    val expectedPairs = Vector[(Person, Person)](Alice -> Bob, Alice -> Charlie)
    val expectedSingle = expectedPairs.mapProj2

    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- insert(CompletedRelation(Alice, Knows, Bob), CompletedRelation(Alice, Knows, Charlie))

          res1 <- findPairs(Knows)
          _ <- assertEqOp(expectedPairs.sorted, res1.sorted, "Write and read pairs failure")
          res2 <- find(Alice >> Knows)
          r <- assertEqOp(expectedSingle.sorted, res2.sorted, "Write and read single failure")
        } yield r
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
    * Check that duplicates of the same relation do not occur twice
    */

  @Test
  def ReadAndWriteSimplePairs(): Unit = {
    val expectedPairs = Vector[(Person, Person)](Alice -> Bob, Alice -> Charlie)
    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- insert(
            CompletedRelation(Alice, Knows, Bob),
            CompletedRelation(Alice, Knows, Bob),
            CompletedRelation(Alice, Knows, Charlie)
          )
          res1 <- findPairs(Knows)
          r <- assertEqOp(expectedPairs.sorted, res1.sorted, "Write duplicates failure")
        } yield r
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