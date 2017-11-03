package unit.suites

import core.CompletedRelation
import core.dsl.Commands.{findPairs, insert}
import core.dsl.RelationSyntax._
import db.interfaces.Empty
import db.using
import org.junit.Test
import unit.Objects._
import unit.{Knows, Owns, Person, assertEqOp, description}

import scala.concurrent.Await
import scala.concurrent.duration._
import scalaz.{-\/, \/-}

trait Transitive { self: HasBackend =>
  /**
    * Check that basic transitive queries work
    */

  @Test
  def SimpleTransitive(): Unit = {
    val expectedPairs = Vector[(Person, Person)](Alice -> Charlie)
    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- insert(Set(
            CompletedRelation(Alice, Knows, Bob),
            CompletedRelation(Bob, Knows, Charlie),
            CompletedRelation(Alice, Knows, David)
          ))
          res1 <- findPairs(Knows -->--> Knows)
          r <- assertEqOp(expectedPairs, res1)
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
    * Check that reverse transitive queries work
    */

  @Test
  def ReverseTransitive(): Unit = {
    val expectedPairs = Vector(Alice -> Charlie, Charlie -> Alice, Alice -> Bob, Bob -> Alice)

    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- insert(Set(
            CompletedRelation(Alice, Owns, Bentley),
            CompletedRelation(Alice, Owns, Ford),
            CompletedRelation(Alice, Owns, VW),
            CompletedRelation(Charlie, Owns, Ford),
            CompletedRelation(Bob, Owns, VW),
            CompletedRelation(Fred, Owns, Mercedes)
          ))

          res1 <- findPairs(Owns --><-- Owns)
          r <- assertEqOp(expectedPairs, res1)
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
    * Check pattern matching transitive and reverse transitive queries work
    */

  @Test
  def RestrictedTransitive(): Unit = {
    val expectedRevPairs = Vector(Alice -> Bob, Bob -> Alice)

    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- insert(Set(
            CompletedRelation(Alice, Owns, Bentley),
            CompletedRelation(Alice, Owns, Ford),
            CompletedRelation(Alice, Owns, VW),
            CompletedRelation(Charlie, Owns, Ford),
            CompletedRelation(Bob, Owns, VW),
            CompletedRelation(Fred, Owns, Mercedes)
          ))

          _ <- insert(Set(
            CompletedRelation(Alice, Knows, Charlie),
            CompletedRelation(Bob, Knows, Charlie),
            CompletedRelation(Alice, Knows, David),
            CompletedRelation(David, Knows, Fred)
          ))

          res1 <- findPairs(Owns --> VW <-- Owns)
          res2 <- findPairs(Knows --> Charlie --> Knows)

          _ <- assertEqOp(expectedRevPairs, res1)
          r <- assertEqOp(expectedRevPairs, res2)
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