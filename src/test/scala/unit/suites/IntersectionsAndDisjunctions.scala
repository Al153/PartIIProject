package unit.suites

import core.CompletedRelation
import core.dsl.Commands._
import core.dsl.RelationSyntax._
import db.interfaces.Empty
import db.using
import org.junit.Test
import unit.Objects._
import unit.{Knows, Owns, assertEqOp, description}

import scala.concurrent.Await
import scala.concurrent.duration._
import scalaz.{-\/, \/-}

trait IntersectionsAndDisjunctions { self: HasBackend =>

  /**
    * Test that Intersections and disjunctions work
    */


  @Test
  def IntersectionsAndDisjunctions(): Unit = {
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
    val expectedIntersection = Set(Alice -> Charlie)

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

          res4 <- findPairs(Knows | (Owns --><-- Owns))
          res3 <- findPairs(Knows & (Owns --><-- Owns))
          res2 <- findPairsDistinct(Knows | (Owns --><-- Owns))
          res1 <- findPairsDistinct((Owns --><-- Owns) & Knows)

          _ <- assertEqOp(expectedIntersection, res4, "union Failure (All)")
          _ <- assertEqOp(expectedIntersection, res3, "intersection Failure (All)")
          _ <- assertEqOp(expectedIntersection, res1, "Intersection failure (Distinct)")
          _ <- assertEqOp(expectedUnion, res2, "Union failure, (Distinct)")
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
    * Test that Intersections by a unary query work
    */
  // todo: implement
}