package unit.suites.individual

import core.user.dsl._
import org.junit.Test
import unit.Objects._
import unit.{Knows, Owns, Person, assertEqOp, description}

trait Transitive { self: HasBackend =>
  /**
    * Check that basic transitive queries work
    */

  @Test
  def SimpleTransitive(): Unit = runTest { implicit instance =>
    val expectedPairs = Vector[(Person, Person)](Alice -> Charlie)
    using(instance) {
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
  }

  /**
    * Check that reverse transitive queries work
    */

  @Test
  def ReverseTransitive(): Unit = runTest { implicit instance =>
    val expectedPairs = Vector(Alice -> Charlie, Charlie -> Alice, Alice -> Bob, Bob -> Alice)
    using(instance) {
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
  }


  /**
    * Check pattern matching transitive and reverse transitive queries work
    */

  @Test
  def RestrictedTransitive(): Unit = runTest { implicit instance =>
    val expectedOwnsPairs = Vector(Bob -> Alice, Alice -> Bob) // this is the ordering produced by memory core.backend - may need to make ordering independent
    val expectedKnowsPairs = Vector(Alice -> Bob)
    using(instance) {
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
  }
}