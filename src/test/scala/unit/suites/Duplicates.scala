package unit.suites

import core.CompletedRelation
import core.dsl.Commands._
import core.dsl.NodeSyntax._
import core.dsl.RelationSyntax._
import db.interfaces.Empty
import db.using
import org.junit.Test
import unit.Objects.{Alice, Bob, Charlie}
import unit.{Knows, assertEqOp, description}

import scala.concurrent.Await
import scala.concurrent.duration._
import scalaz.{-\/, \/-}

trait Duplicates { self: HasBackend =>
  /**
    * Check difference between findAll and findDistinct
  */

  @Test
  def PairsVsDistinct(): Unit = {
    val expectedDistinctPairs = Set(Alice -> Charlie)
    val expectedAllPairs = Vector(Alice -> Charlie, Alice -> Charlie)

    val expectedDistinctSingle = Set(Charlie)
    val expectedAllSingle = Vector(Charlie, Charlie)

    val op = using(backend.open(Empty, description)) {
      implicit instance =>
        for {
          _ <- insert(Set(
            CompletedRelation(Alice, Knows, Bob), // there are two routes from Alice to Charlie
            CompletedRelation(Bob, Knows, Charlie),
            CompletedRelation(Alice, Knows, Charlie)
          ))

          res1 <- findPairs(Knows -->--> Knows)
          res2 <- findPairsDistinct(Knows -->--> Knows)
          res3 <- find(Alice >> (Knows -->--> Knows))
          res4 <- findDistinct(Alice >> (Knows -->--> Knows))

          _ <- assertEqOp(expectedAllPairs, res1)
          _ <- assertEqOp(expectedDistinctPairs, res2)
          _ <- assertEqOp(expectedAllSingle, res3)
          _ <- assertEqOp(expectedDistinctSingle, res4)
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