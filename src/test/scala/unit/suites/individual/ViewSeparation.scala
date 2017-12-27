package unit.suites.individual

import core.backend._
import core.backend.interfaces._
import core.dsl.Commands._
import core.relations.CompletedRelation
import org.junit.Test
import unit.Objects._
import unit._

import scala.concurrent.Await
import scala.concurrent.duration._
import scalaz.{-\/, \/-}

/**
  * Created by Al on 27/12/2017.
  */
trait ViewSeparation { self: HasBackend =>


  /**
    * Make sure that separate writes don't interfere with each other
   */
  @Test
  def SeparateWrites: Unit = {
    val expected1 = Vector[(Person, Person)]((Alice, Bob), (Charlie, David))
    val expected2 = Vector[(Person, Person)]((Bob, Fred), (Fred, Charlie), (Fred, Georgie), (Hannah, Ian))

    val op = for {
      instance <- backend.open(Empty, description)
      initialView <- instance.getDefaultView

      v1 <- writeToView(instance, initialView) {
        implicit instance =>
          insert(
            CompletedRelation(Alice, Knows, Bob),
            CompletedRelation(Charlie, Knows, David)
          )
        }

      v2 <- writeToView(instance, initialView) {
        implicit  instance =>
          insert(
            CompletedRelation(Bob, Knows, Fred),
            CompletedRelation(Fred, Knows, Charlie),
            CompletedRelation(Fred, Knows, Georgie),
            CompletedRelation(Hannah, Knows, Ian)
          )
      }

      r1 <- usingView(instance, v1) {
        implicit instance => findPairs(Knows)
      }

      r2 <- usingView(instance, v2) {
        implicit instance => findPairs(Knows)
      }
      _ <- assertEq(expected1.sorted, r1.sorted, "SeparateWrites View 1")
      _ <- assertEq(expected2.sorted, r2.sorted, "SeparateWrites View 2")
    } yield ()

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
    * Make sure that separate writes don't interfere with each other, this time by asking for a zero order repetition
    * of a relation. This should return all objects of the type that are accessible from the view
    */
  @Test
  def SeparateWritesPickAllObjects: Unit = {
    val expected1 = Vector[(Person, Person)]((Alice, Alice), (Bob, Bob), (Charlie, Charlie), (David, David))
    val expected2 = Vector[(Person, Person)]((Bob, Bob), (Fred, Fred), (Charlie, Charlie), (Georgie, Georgie), (Hannah, Hannah), (Ian, Ian))

    val op = for {
      instance <- backend.open(Empty, description)
      initialView <- instance.getDefaultView

      v1 <- writeToView(instance, initialView) {
        implicit instance =>
          insert(
            CompletedRelation(Alice, Knows, Bob),
            CompletedRelation(Charlie, Knows, David)
          )
      }

      v2 <- writeToView(instance, initialView) {
        implicit  instance =>
          insert(
            CompletedRelation(Bob, Knows, Fred),
            CompletedRelation(Fred, Knows, Charlie),
            CompletedRelation(Fred, Knows, Georgie),
            CompletedRelation(Hannah, Knows, Ian)
          )
      }

      r1 <- usingView(instance, v1) {
        implicit instance => findPairs(Knows * 0)
      }

      r2 <- usingView(instance, v2) {
        implicit instance => findPairs(Knows * 0)
      }
      _ <- assertEq(expected1.sorted, r1.sorted, "SeparateWrites Repetition View 1")
      _ <- assertEq(expected2.sorted, r2.sorted, "SeparateWrites View 2")
    } yield ()

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
