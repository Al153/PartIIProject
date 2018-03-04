package remote.tests

import construction.imdb.DBBuilder
import construction.imdb.IMDBSchema.{ActsIn, Person, schemaDescription}
import core.user.containers.{ConstrainedFuture, Path}
import core.user.dsl.{E, HasRecovery}
import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription
import core.utils.Logged
import remote.lmdbOriginal
import remote.tests.PathFindingTest.logger
import core.user.dsl._
import remote.util.TestIndex._
import remote.util.TestName._
import remote.util.{TestIndex, TestName, TestSpec}

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 04/03/2018.
  */
object SingleShortestPath extends TestSpec[Option[Path[Person]]] with Logged {
  override def testName: TestName = "Pathfinding".test

  override def batchSize: TestIndex = 6.tests

  override def setup[ThisE <: E](instance: DBInstance[ThisE])(implicit R: HasRecovery[ThisE], ec: ExecutionContext): ConstrainedFuture[ThisE, Unit] =
    for {
      v0 <- instance.getDefaultView
      v1 <- writeToView(instance, v0){
        DBBuilder.buildDB("imdb/smallest")(instance)
      }

      _ = logger.info("Smallest view: " + v1)

      v2 <- writeToView(instance, v0){
        DBBuilder.buildDB("imdb/small_sparse")(instance)
      }

      _ = logger.info("Small_sparse view: " + v2)

      v3 <- writeToView(instance, v0){
        DBBuilder.buildDB("imdb/small")(instance)
      }

      _ = logger.info("small view: " + v3)

      v4 <- writeToView(instance, v0){
        DBBuilder.buildDB("imdb/medium_sparse")(instance)
      }

      _ = logger.info("medium_sparse view: " + v4)

      v5 <- writeToView(instance, v0){
        DBBuilder.buildDB("imdb/medium")(instance)
      }

      _ = logger.info("medium view: " + v5)

      v6 <- writeToView(instance, v0){
        DBBuilder.buildDB("imdb/large")(instance)
      }

      _ = logger.info("large view: " + v6)

    } yield ()

  override def test[ThisE <: E](instance: DBInstance[ThisE])(index: TestIndex)(implicit R: HasRecovery[ThisE], ec: ExecutionContext): ConstrainedFuture[ThisE, Option[Path[Person]]] = {
    implicit val inst = instance
    for {
      views <- instance.getViews
      v = views.toVector.sortBy(_.id).apply(index.i)
      r <- usingView(instance, v){
        shortestPath(KevinBacon, TomCruise, ActsIn --><-- ActsIn)
      }
      lengths = r.map(_.length: Long)
      _ = r.foreach(p => logger.info("Path = " + p))
      _ = logger.info("Number of paths found = " + r.size)
      _ = if (r.nonEmpty) {
        logger.info("Average Length of paths found = " + lengths.sum/r.size)
        logger.info("longest path = " + lengths.max)
        logger.info("shortest path = " + lengths.min)
      }
    } yield r
  }

  override def schema: SchemaDescription = schemaDescription
  override def ignoreBackends: Set[String] = Set(lmdbOriginal)
}
