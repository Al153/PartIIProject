package unit.suites

import core.CompletedRelation
import core.dsl.Commands.{find, findPairs, insert}
import core.dsl.NodeSyntax._
import db.interfaces.Empty
import db.using
import org.junit.Test
import unit.Objects._
import unit.{Knows, Person, assertEqOp, description}
import utils._

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
          _ <- insert(Set(CompletedRelation(Alice, Knows, Bob)))
          _ <- insert(Set(CompletedRelation(Alice, Knows, Charlie)))

          res1 <- findPairs(Knows)
          _ <- assertEqOp(expectedPairs, res1, "Write and read pairs failure")
          res2 <- find(Alice >> Knows)
          r <- assertEqOp(expectedSingle, res2, "Write and read single failure")
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
          _ <- insert(Set(
            CompletedRelation(Alice, Knows, Bob),
            CompletedRelation(Alice, Knows, Bob),
            CompletedRelation(Alice, Knows, Charlie)
          ))
          res1 <- findPairs(Knows)
          r <- assertEqOp(expectedPairs, res1, "Write duplicates failure")
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