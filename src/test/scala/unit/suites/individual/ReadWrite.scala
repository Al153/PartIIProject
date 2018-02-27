package unit.suites.individual

import core.user.dsl._
import core.utils._
import org.junit.Test
import unit.Objects._
import unit._

trait ReadWrite[E1 <: E] { self: HasBackend[E1] =>
  /**
    * A write followed by a read should result in the written values being read
    */

  @Test
  def WriteAndReadPair(): Unit = runTest {implicit instance =>
    val expectedPairs = Set(Alice -> Bob, Alice -> Charlie)
    val expectedSingle = expectedPairs.mapProj2

    using(instance) {
      for {
        _ <- insert(CompletedRelation(Alice, Knows, Bob), CompletedRelation(Alice, Knows, Charlie))
        res1 <- findPairs(Knows)
        _ <- assertEqOp(expectedPairs, res1, "Write and read pairs failure")
        res2 <- find(Alice >> Knows)
        r <- assertEqOp(expectedSingle, res2, "Write and read single failure")
      } yield r
    }
  }

  /**
    * Check that duplicates of the same relation do not occur twice
    */

  @Test
  def ReadAndWriteSimplePairs(): Unit = runTest { implicit instance =>
    val expectedPairs = Set[(Person, Person)](Alice -> Bob, Alice -> Charlie)
    using(instance) {
      for {
        _ <- insert(
          CompletedRelation(Alice, Knows, Bob),
          CompletedRelation(Alice, Knows, Bob),
          CompletedRelation(Alice, Knows, Charlie)
        )
        res1 <- findPairs(Knows)
        r <- assertEqOp(expectedPairs, res1, "Write duplicates failure")
      } yield r
    }
  }

  @Test
  def findSingles(): Unit = runTest { implicit  instance =>
    val expected = Set[Person](Alice, Bob, Charlie)
    using(instance) {
      for {
        _ <- insert(
          CompletedRelation(Alice, Knows, Bob),
          CompletedRelation(Alice, Knows, Bob),
          CompletedRelation(Alice, Knows, Charlie)
        )
        res1 <- find(personSchema.any)
        res2 <- find(personSchema.any)
        _ <- assertEqOp(expected, res1, "Find singles failure")
        _ <- assertEqOp(expected, res2, "Find Singles set failure")
      } yield ()
    }

  }
}