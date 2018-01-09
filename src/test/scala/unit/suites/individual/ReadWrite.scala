package unit.suites.individual

import core.user.dsl._
import core.utils._
import core.user.schema._
import org.junit.Test
import unit.Objects._
import unit._

trait ReadWrite { self: HasBackend =>
  /**
    * A write followed by a read should result in the written values being read
    */

  @Test
  def WriteAndReadPair(): Unit = runTest {implicit instance =>
    val expectedPairs = Vector[(Person, Person)](Alice -> Bob, Alice -> Charlie)
    val expectedSingle = expectedPairs.mapProj2

    using(instance) {
      for {
        _ <- insert(CompletedRelation(Alice, Knows, Bob), CompletedRelation(Alice, Knows, Charlie))
        res1 <- findPairs(Knows)
        _ <- assertEqOp(expectedPairs.sorted, res1.sorted, "Write and read pairs failure")
        res2 <- find(Alice >> Knows)
        r <- assertEqOp(expectedSingle.sorted, res2.sorted, "Write and read single failure")
      } yield r
    }
  }

  /**
    * Check that duplicates of the same relation do not occur twice
    */

  @Test
  def ReadAndWriteSimplePairs(): Unit = runTest { implicit instance =>
    val expectedPairs = Vector[(Person, Person)](Alice -> Bob, Alice -> Charlie)
    using(instance) {
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
  }

  @Test
  def findSingles(): Unit = runTest { implicit  instance =>
    val expected = Vector[Person](Alice, Bob, Charlie)
    using(instance) {
      for {
        _ <- insert(
          CompletedRelation(Alice, Knows, Bob),
          CompletedRelation(Alice, Knows, Bob),
          CompletedRelation(Alice, Knows, Charlie)
        )
        res1 <- find(personSchema.any)
        res2 <- findDistinct(personSchema.any)
        _ <- assertEqOp(expected.sorted, res1.sorted, "Find singles failure")
        _ <- assertEqOp(expected.toSet, res2, "Find Singles set failure")
      } yield ()
    }

  }
}