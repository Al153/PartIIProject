package unit.suites.individual

import core.user.dsl._
import org.junit.Test
import unit.Objects.{Alice, Bob, Charlie, David}
import unit.{Knows, assertEqOp, description}

trait Duplicates { self: HasBackend =>
  /**
    * Check difference between findAll and findDistinct
  */

  @Test
  def PairsVsDistinct(): Unit = runTest { implicit instance =>
    val expectedDistinctPairs = Set(Alice -> Charlie)
    val expectedAllPairs = Vector(Alice -> Charlie, Alice -> Charlie)

    val expectedDistinctSingle = Set(Charlie)
    val expectedAllSingle = Vector(Charlie, Charlie)
    using(instance) {

      for {
        _ <- insert(
          CompletedRelation(Alice, Knows, Bob), // there are two routes from Alice to Charlie
          CompletedRelation(Bob, Knows, Charlie),
          CompletedRelation(Alice, Knows, David),
          CompletedRelation(David, Knows, Charlie)
        )

        res1 <- findPairs(Knows -->--> Knows)
        res2 <- findPairsDistinct(Knows -->--> Knows)
        res3 <- find(Alice >> (Knows -->--> Knows))
        res4 <- findDistinct(Alice >> (Knows -->--> Knows))

        _ <- assertEqOp(expectedAllPairs, res1, "All pairs failure")
        _ <- assertEqOp(expectedDistinctPairs, res2, "Distinct pairs failure")
        _ <- assertEqOp(expectedAllSingle, res3, "All single pairs failure")
        _ <- assertEqOp(expectedDistinctSingle, res4, "Distinct single failure")
      } yield ()
    }
  }
}