package unit.suites.individual

import core.user.dsl._
import org.junit.Test
import unit.Objects.{Alice, Bob, Charlie, David}
import unit.{Knows, assertEqOp, description}

trait Duplicates { self: HasBackend =>
  /**
    * Check the number of paths is always 0 or 1 (redundant with the usage of sets)
  */

  @Test
  def Pairs(): Unit = runTest { implicit instance =>
    val expectedDistinctPairs = Set(Alice -> Charlie)

    val expectedDistinctSingle = Set(Charlie)
    using(instance) {

      for {
        _ <- insert(
          CompletedRelation(Alice, Knows, Bob), // there are two routes from Alice to Charlie
          CompletedRelation(Bob, Knows, Charlie),
          CompletedRelation(Alice, Knows, David),
          CompletedRelation(David, Knows, Charlie)
        )

        res1 <- findPairs(Knows -->--> Knows)
        res3 <- find(Alice >> (Knows -->--> Knows))

        _ <- assertEqOp(expectedDistinctPairs, res1, "Distinct pairs failure")
        _ <- assertEqOp(expectedDistinctSingle, res3, "Distinct single failure")
      } yield ()
    }
  }


  @Test
  def Distinct(): Unit = runTest { implicit instance =>
    val expectedDistinctPairs = Set(Alice -> Charlie)
    val expectedDistinctSingle = Set(Charlie)
    using(instance) {

      for {
        _ <- insert(
          CompletedRelation(Alice, Knows, Bob), // there are two routes from Alice to Charlie
          CompletedRelation(Bob, Knows, Alice),
          CompletedRelation(Alice, Knows, David),
          CompletedRelation(David, Knows, Charlie)
        )

        res1 <- findPairs((Knows -->--> Knows).distinct)
        res3 <- find(Alice >> (Knows -->--> Knows).distinct)

        _ <- assertEqOp(expectedDistinctPairs, res1, "Distinct pairs failure")
        _ <- assertEqOp(expectedDistinctSingle, res3, "Distinct single failure")
      } yield ()
    }
  }
}