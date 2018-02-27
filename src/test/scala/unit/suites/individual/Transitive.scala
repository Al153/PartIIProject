package unit.suites.individual

import core.user.containers.Operation
import core.user.dsl._
import core.user.interfaces.DBInstance
import core.user.schema.SchemaObject
import org.junit.Test
import core.utils._
import unit.Objects._
import unit.{Knows, Owns, Person, assertEqOp}

import scala.concurrent.ExecutionContext

trait Transitive[E1 <: E] { self: HasBackend[E1] =>
  /**
    * Check that basic transitive queries work
    */

  @Test
  def SimpleTransitive(): Unit = runTest { implicit instance =>
    val expectedPairs = Set(Alice -> Charlie)
    using(instance) {
      for {
        _ <- insert(
          CompletedRelation(Alice, Knows, Bob),
          CompletedRelation(Bob, Knows, Charlie),
          CompletedRelation(Alice, Knows, David)
        )
        res1 <- findPairs(Knows -->--> Knows)
        res2 <- findPairs(Knows -->--> Knows)
        _ <- assertEqOp(expectedPairs, res1, "Simple transitive failure (all)")
        _ <- assertEqOp(expectedPairs, res2, "Simple transitive failure (distinct)")
      } yield ()
    }
  }

  /**
    * Check that reverse transitive queries work
    */

  @Test
  def ReverseTransitive(): Unit = runTest { implicit instance =>
    val expectedPairs = Set(Alice -> Charlie, Charlie -> Alice, Alice -> Bob, Bob -> Alice)
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
        res2 <- findPairs(Owns --><-- Owns)
        _ <- assertEqOp(expectedPairs, res1, "Reverse transitive failure (all)")
        _ <- assertEqOp(expectedPairs, res2, "Reverse transitive failure (distinct)")
      } yield ()
    }
  }


  /**
    * Check pattern matching transitive and reverse transitive queries work
    */

  @Test
  def RestrictedTransitive(): Unit = runTest { implicit instance =>
    val expectedOwnsPairs = Set(Bob -> Alice, Alice -> Bob) // this is the ordering produced by memory core.backend - may need to make ordering independent
    val expectedKnowsPairs = Set(Alice -> Bob)
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
        res3 <- findPairs(Owns --> VW <-- Owns)
        res4 <- findPairs(Knows --> Charlie --> Knows)

        _ <- assertEqOp(expectedOwnsPairs, res1, "Restricted reverse transitive failed")
        _ <- assertEqOp(expectedKnowsPairs, res2, "Restricted transitive failed")
        _ <- assertEqOp(expectedOwnsPairs, res3, "Restricted reverse transitive distinct failed")
        _ <- assertEqOp(expectedKnowsPairs, res4, "Restricted transitive distinct  failed")
        } yield ()
    }
  }

  /**
    * Check that more complex transitive queries work (including those with lots of redundancy)
    */

  @Test
  def RepeatedTransitive(): Unit = runTest { implicit instance =>
    val expectedPairs = bigUnion(Seq(
      Set(Charlie, Eve, Fred, Georgie, Hannah, Ian).map(Alice -> _),
      Set(Hannah, Fred, Ian).map(Bob -> _),
      Set(Charlie -> Ian),
      Set(Fred, Hannah, Ian).map(David -> _),
      Set(Georgie -> Ian),
      Set(Eve -> Ian)
    ))

    using(instance) {
      for {
        _ <- setupPath
        secondOrderKnows = (Knows -->--> Knows) | Knows
        res1 <- findPairs(secondOrderKnows -->--> secondOrderKnows)
        res2 <- findPairs(secondOrderKnows -->--> secondOrderKnows)
        _ <- assertEqOp(expectedPairs, res1, s"Simple transitive failure (all), diff = ${res1.diff(expectedPairs)}")
        _ <- assertEqOp(expectedPairs, res2, "Simple transitive failure (distinct)")
      } yield ()
    }
  }

  /**
    *
    * Set up a grid:
    *
    *  A -> B -> C
    *  |    |    |
    * \/   \/   \/
    *  D -> E -> F
    *  |    |    |
    * \/   \/   \/
    *  G -> H -> I
    *
    *
    */

  private def setupPath(implicit instance: DBInstance[E1], ec: ExecutionContext, sa: SchemaObject[Person]): Operation[E1, Unit] =
    insert(
      CompletedRelation(Alice, Knows, Bob), CompletedRelation(Bob, Knows, Charlie),
      CompletedRelation(Alice, Knows, David), CompletedRelation(David, Knows, Eve),
      CompletedRelation(Bob, Knows, Eve), CompletedRelation(Eve, Knows, Fred),
      CompletedRelation(Charlie, Knows, Fred), CompletedRelation(David, Knows, Georgie),
      CompletedRelation(Georgie, Knows, Hannah), CompletedRelation(Eve, Knows, Hannah),
      CompletedRelation(Hannah, Knows, Ian), CompletedRelation(Fred, Knows, Ian)
    )
}