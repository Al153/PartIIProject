package remote.tests

import construction.imdb.DBBuilder
import construction.imdb.IMDBSchema.{ActsIn, Person, schemaDescription}
import core.user.containers.{ConstrainedFuture, Path}
import core.user.dsl.{E, allShortestPaths, usingView, writeToView}
import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription
import remote.TestIndex._
import remote.TestName._
import remote.{TestIndex, TestName, TestSpec}

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 30/01/2018.
  */
object PathFindingTest extends TestSpec[Set[Path[Person]]]{
  override def testName: TestName = "Pathfinding".test

  override def batchSize: TestIndex = 6.tests

  override def setup(instance: DBInstance)(implicit ec: ExecutionContext): ConstrainedFuture[E, Unit] =
    for {
      v0 <- instance.getDefaultView
      _ <- writeToView(instance, v0){
        DBBuilder.buildDB("imdb/smallest")(instance)
      }

      _ <- writeToView(instance, v0){
        DBBuilder.buildDB("imdb/small_sparse")(instance)
      }

      _ <- writeToView(instance, v0){
        DBBuilder.buildDB("imdb/small")(instance)
      }

      _ <- writeToView(instance, v0){
        DBBuilder.buildDB("imdb/medium_sparse")(instance)
      }

      _ <- writeToView(instance, v0){
        DBBuilder.buildDB("imdb/medium")(instance)
      }

      _ <- writeToView(instance, v0){
        DBBuilder.buildDB("imdb/large")(instance)
      }
    } yield ()

  override def test(instance: DBInstance)(index: TestIndex)(implicit ec: ExecutionContext): ConstrainedFuture[E, Set[Path[Person]]] = {
    implicit val inst = instance
    for {
      views <- instance.getViews
      v = views.toVector.apply(index.i)
      r <- usingView(instance, v){
        allShortestPaths(KevinBacon, ActsIn --><-- ActsIn)
      }
    } yield r
  }

  override def schema: SchemaDescription = schemaDescription






}
