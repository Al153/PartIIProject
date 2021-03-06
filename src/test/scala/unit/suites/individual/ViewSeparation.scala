package unit.suites.individual

import core.user.containers.Path
import core.user.dsl._
import org.junit.Test
import unit.Objects._
import unit._

import scalaz.Scalaz._
import scalaz._
/**
  * Created by Al on 27/12/2017.
  */
trait ViewSeparation[E1 <: E] { self: HasBackend[E1] =>


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
      _ <- assertEq[E1, Set[(Person, Person)]](expected1, r1, "SeparateWrites View 1")
      _ <- assertEq[E1, Set[(Person, Person)]](expected2, r2, "SeparateWrites View 2")
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
      _ <- assertEq[E1, Set[(Person, Person)]](expected1, r1, "SeparateWrites Repetition View 1")
      _ <- assertEq[E1, Set[(Person, Person)]](expected2, r2, "SeparateWrites View 2")
    } yield ()
  }

  /**
    * Make sure that pattern lookups are unaffected by other views
    */

  @Test
  def SeparatePatterns(): Unit = runTest { implicit instance =>
    val expectedPerson = Set[Person](Alice)
    val expectedPet = Set[Pet]()

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

      r2 <- usingView(instance, v1) {
        find(personSchema.pattern(None))
      }

      _ <- assertEq[E1, Set[Pet]](expectedPet, r1, "Separate patterns pet")
      _ <- assertEq[E1, Set[Person]](expectedPerson, r2, "Separate Patterns")
    } yield ()
  }

  /**
    * Make sure that separate writes don't interfere with each other, this time by asking for a zero order repetition
    * of a relation. This should return all objects of the type that are accessible from the view
    */
  @Test
  def SeparateUpto: Unit = runTest { implicit instance =>
    val expected1 = Set(Alice -> Alice, Bob -> Bob, Charlie -> Charlie, David -> David, Alice -> Bob, Charlie -> David)
    val expected2 =
      Set(Bob, Fred).map(Bob -> _)
        .union(Set(Fred, Charlie, Georgie).map(Fred -> _))
        .union(Set(Hannah, Ian).map(Hannah -> _))
        .union(Set(Charlie -> Charlie, Georgie -> Georgie, Ian -> Ian))

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
        findPairs(Knows * (0 --> 1))
      }

      r2 <- usingView(instance, v2) {
        findPairs(Knows * (0 --> 1))
      }
      _ <- assertEq[E1, Set[(Person, Person)]](expected1, r1, "SeparateWrites Upto View 1")
      _ <- assertEq[E1, Set[(Person, Person)]](expected2, r2, "SeparateWrites Upto View 2")
    } yield ()
  }


  /**
    * Make sure that separate writes don't interfere with each other, this time by asking for a zero order repetition
    * of a relation. This should return all objects of the type that are accessible from the view
    */
  @Test
  def SeparateFixedPoint: Unit = runTest { implicit instance =>
    val expected1 = Set(Alice -> Alice, Bob -> Bob, Charlie -> Charlie, David -> David, Alice -> Bob, Charlie -> David)
    val expected2 =
      Set(Bob, Fred, Charlie, Georgie, Hannah, Ian).map(x => x -> x)
        .union(Set(Fred, Charlie, Georgie).map(Bob -> _))
        .union(Set(Charlie, Georgie).map(Fred -> _))
        .union(Set(Hannah -> Ian))

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
        findPairs(Knows.**)
      }

      r2 <- usingView(instance, v2) {
        findPairs(Knows.**)
      }
      _ <- assertEq[E1, Set[(Person, Person)]](expected1, r1, "SeparateWrites Repetition View 1")
      _ <- assertEq[E1, Set[(Person, Person)]](expected2, r2, "SeparateWrites Repetition View 2")
    } yield ()
  }


  /**
    * Make sure that separate writes don't interfere with each other, this time by querying for a path from an object that doesn't exist
    * in this view. This should return all objects of the type that are accessible from the view
    */
  @Test
  def SeparatePathFinding: Unit = runTest { implicit instance =>
    val expected1 = Set[Path[Person]]()
    val expected2 = None
    val expected3 = None

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
        allShortestPaths(Fred, Knows)
      }

      r2 <- usingView(instance, v2) {
        shortestPath(Alice, Fred, Knows)
      }

      r3 <- usingView(instance, v2) {
        shortestPath(Fred, Alice, Knows)
      }
      _ <- assertEq[E1, Set[Path[Person]]](expected1, r1, "SeparateWrites Pathfinding 1")
      _ <- assertEq[E1, Option[Path[Person]]](expected2, r2, "SeparateWrites Pathfinding 2")
      _ <- assertEq[E1, Option[Path[Person]]](expected3, r3, "SeparateWrites Pathfinding 3")
    } yield ()
  }
}
