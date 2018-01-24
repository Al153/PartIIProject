package unit.suites.individual

import core.user.dsl._
import org.junit.Test
import unit.Objects._
import unit._

import scalaz.Scalaz._
import scalaz._
/**
  * Created by Al on 27/12/2017.
  */
trait ViewSeparation { self: HasBackend =>


  /**
    * Make sure that separate writes don't interfere with each other
   */
  @Test
  def SeparateWrites: Unit = runTest { implicit instance =>
    val expected1 = Set(Alice -> Bob, Charlie -> David)
    val expected2 = Set(Bob -> Fred, Fred -> Charlie, Fred -> Georgie, Hannah -> Ian)

    for {
      initialView <- instance.getDefaultView

      v1 <- writeToView(instance, initialView) {
        insert(
          CompletedRelation(Alice, Knows, Bob),
          CompletedRelation(Charlie, Knows, David)
        )
      }

      v2 <- writeToView(instance, initialView) {
        insert(
          CompletedRelation(Bob, Knows, Fred),
          CompletedRelation(Fred, Knows, Charlie),
          CompletedRelation(Fred, Knows, Georgie),
          CompletedRelation(Hannah, Knows, Ian)
        )
      }

      r1 <- usingView(instance, v1) {
        findPairs(Knows)
      }

      r2 <- usingView(instance, v2) {
        findPairs(Knows)
      }
      _ <- assertEq(expected1, r1, "SeparateWrites View 1")
      _ <- assertEq(expected2, r2, "SeparateWrites View 2")
    } yield ()
  }

  /**
    * Make sure that separate writes don't interfere with each other, this time by asking for a zero order repetition
    * of a relation. This should return all objects of the type that are accessible from the view
    */
  @Test
  def SeparateWritesPickAllObjects: Unit = runTest { implicit instance =>
    val expected1 = Set((Alice, Alice), (Bob, Bob), (Charlie, Charlie), (David, David))
    val expected2 = Set((Bob, Bob), (Fred, Fred), (Charlie, Charlie), (Georgie, Georgie), (Hannah, Hannah), (Ian, Ian))

    for {
      initialView <- instance.getDefaultView

      v1 <- writeToView(instance, initialView) {
        insert(
          CompletedRelation(Alice, Knows, Bob),
          CompletedRelation(Charlie, Knows, David)
        )
      }

      v2 <- writeToView(instance, initialView) {
        insert(
          CompletedRelation(Bob, Knows, Fred),
          CompletedRelation(Fred, Knows, Charlie),
          CompletedRelation(Fred, Knows, Georgie),
          CompletedRelation(Hannah, Knows, Ian)
        )
      }

      r1 <- usingView(instance, v1) {
        findPairs(Knows * 0)
      }

      r2 <- usingView(instance, v2) {
        findPairs(Knows * 0)
      }
      _ <- assertEq(expected1, r1, "SeparateWrites Repetition View 1")
      _ <- assertEq(expected2, r2, "SeparateWrites View 2")
    } yield ()
  }

  /**
    * Make sure that pattern lookups are unaffected by other views
    */

  @Test
  def SeparatePatterns(): Unit = runTest { implicit instance =>
    val expected = Set[Person]()

    for {
      initialView <- instance.getDefaultView

      v1 <- writeToView(instance, initialView) {
        insert(
          CompletedRelation(gus, OwnedBy, Alice)
        )
      }

      v2 <- writeToView(instance, initialView) {
        insert( // no animals in this view
          CompletedRelation(Bob, Knows, Fred)
        )
      }

      r1 <- usingView(instance, v2) {
        find(petSchema.pattern("Gus".some, None, None, None))
      }

      _ <- assertEq(expected, r1, "Separate Patterns")
    } yield ()

  }
}
