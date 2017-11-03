package unit.suites

import core.CompletedRelation
import core.dsl.Commands.{findPairsDistinct, insert}
import core.dsl.RelationSyntax._
import db.interfaces.Empty
import db.using
import org.junit.Test
import unit.Objects._
import unit.{Knows, Owns, assertEqOp, description}

import scala.concurrent.Await
import scala.concurrent.duration._
import scalaz.{-\/, \/-}

trait ConjunctionsAndDisjunctions { self: HasBackend =>

  /**
    * Test that conjunctions and disjunctions work
    */


  @Test
  def ConjunctionsAndDisjunctions(): Unit = {
    val expectedUnion = Set(
      Alice -> Charlie,
      Alice -> Bob,
      Alice -> Fred,
      Bob -> Eve,
      Alice -> Eve,
      Eve -> Alice,
      Charlie -> Eve,
      Eve -> Charlie,
      Fred -> Bob,
      Bob -> Fred
    )
    val expectedConjunction = Set(Alice -> Charlie)

    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- insert(Set(
            CompletedRelation(Alice, Knows, Bob),
            CompletedRelation(Alice, Knows, Charlie),
            CompletedRelation(Alice, Knows, Fred),
            CompletedRelation(Bob, Knows, Eve)
          ))

          _ <- insert(Set (
            CompletedRelation(Alice, Owns, Ford),
            CompletedRelation(Charlie, Owns, Ford),
            CompletedRelation(Eve, Owns, Ford),
            CompletedRelation(Fred, Owns, Mercedes),
            CompletedRelation(Bob, Owns, Mercedes)
          ))

          res1 <- findPairsDistinct(Knows & (Owns --><-- Owns))
          res2 <- findPairsDistinct(Knows | (Owns --><-- Owns))



          _ <- assertEqOp(expectedConjunction, res1)
          _ <- assertEqOp(expectedUnion, res2)
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
    * Test that Conjunctions by a unary query work
    */
  // todo: implement
}