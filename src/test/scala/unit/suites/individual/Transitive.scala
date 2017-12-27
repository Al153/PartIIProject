package unit.suites.individual

import core.backend.interfaces.Empty
import core.backend.using
import core.dsl.Commands._
import core.dsl.RelationSyntax._
import core.relations.CompletedRelation
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
          _ <- insert(
            CompletedRelation(Alice, Knows, Bob),
            CompletedRelation(Bob, Knows, Charlie),
            CompletedRelation(Alice, Knows, David)
          )
          res1 <- findPairs(Knows -->--> Knows)
          res2 <- findPairsDistinct(Knows -->--> Knows)
          _ <- assertEqOp(expectedPairs, res1, "Simple transitive failure (all)")
          _ <- assertEqOp(expectedPairs.toSet, res2, "Simple transitive failure (distinct)")
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
    * Check that reverse transitive queries work
    */

  @Test
  def ReverseTransitive(): Unit = {
    val expectedPairs = Vector(Alice -> Charlie, Charlie -> Alice, Alice -> Bob, Bob -> Alice)

    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- insert(
            CompletedRelation(Alice, Owns, Bentley),
            CompletedRelation(Alice, Owns, Ford),
            CompletedRelation(Alice, Owns, VW),
            CompletedRelation(Charlie, Owns, Ford),
            CompletedRelation(Bob, Owns, VW),
            CompletedRelation(Fred, Owns, Mercedes)
          )

          res1 <- findPairs(Owns --><-- Owns)
          res2 <- findPairsDistinct(Owns --><-- Owns)
          _ <- assertEqOp(expectedPairs.sorted, res1.sorted, "Reverse transitive failure (all)")
          _ <- assertEqOp(expectedPairs.toSet, res2, "Reverse transitive failure (distinct)")
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
    * Check pattern matching transitive and reverse transitive queries work
    */

  @Test
  def RestrictedTransitive(): Unit = {
    val expectedOwnsPairs = Vector(Bob -> Alice, Alice -> Bob) // this is the ordering produced by memory core.backend - may need to make ordering independent
    val expectedKnowsPairs = Vector(Alice -> Bob)

    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- insert(
            CompletedRelation(Alice, Owns, Bentley),
            CompletedRelation(Alice, Owns, Ford),
            CompletedRelation(Alice, Owns, VW),
            CompletedRelation(Charlie, Owns, Ford),
            CompletedRelation(Bob, Owns, VW),
            CompletedRelation(Fred, Owns, Mercedes)
          )

          _ <- insert(
            CompletedRelation(Alice, Knows, Charlie),
            CompletedRelation(Charlie, Knows, Bob),
            CompletedRelation(Alice, Knows, David),
            CompletedRelation(David, Knows, Fred)
          )

          res1 <- findPairs(Owns --> VW <-- Owns)
          res2 <- findPairs(Knows --> Charlie --> Knows)
          res3 <- findPairsDistinct(Owns --> VW <-- Owns)
          res4 <- findPairsDistinct(Knows --> Charlie --> Knows)



          _ <- assertEqOp(expectedOwnsPairs.sorted, res1.sorted, "Restricted reverse transitive failed")
          _ <- assertEqOp(expectedKnowsPairs.sorted, res2.sorted, "Restricted transitive failed")
          _ <- assertEqOp(expectedOwnsPairs.toSet, res3, "Restricted reverse transitive distinct failed")
          _ <- assertEqOp(expectedKnowsPairs.toSet, res4, "Restricted transitive distinct  failed")

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