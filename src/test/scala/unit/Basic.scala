package unit

import core.concrete.relations.CompletedRelation
import core.containers.ConstrainedFuture
import core.error.E
import db.interfaces.DBInstance
import db.using
import core.dsl.Commands._
import core.dsl.NodeSyntax._
import utils._

import scalaz.\/

/**
 * This is a set of basic tests  for a given backend
 */

class Basic(val instance: ConstrainedFuture[E, DBInstance]) {

  val Jim = Person("Jim")
  val Bob = Person("Bob")
  val John = Person("John")

  def WriteAndReadPair: ConstrainedFuture[E, Unit] = {
    val expectedPairs = Vector[(Person, Person)](Jim -> Bob, Jim -> John)
    val expectedSingle = expectedPairs.mapProj2

    using(instance) {
      implicit instance =>
        for {
          _ <- insert(Set(CompletedRelation(Jim, Knows, Bob)))
          _ <- insert(Set(CompletedRelation(Jim, Knows, Bob)))
          res1 <- findPairs(Knows.tree)
          _ <- assertEqOp(expectedPairs, res1)
          res2 <- find(Jim.reachableWith(Knows))
          r <- assertEqOp(expectedSingle, res2)
        } yield r
    }
  }

  def ReadAndWriteDistinct: ConstrainedFuture[E, Unit]

}