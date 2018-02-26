package remote.tests

import construction.imdb.DBBuilder
import construction.imdb.IMDBSchema.{ActsIn, Person, schemaDescription}
import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, using, _}
import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription
import remote.util.TestIndex._
import remote.util.TestName._
import remote.util.{TestIndex, TestSpec}
import remote.{lmdbOriginal, logger}

import scala.concurrent.ExecutionContext

object ExactlyPairs extends TestSpec[Set[(Person, Person)]] {
  override def testName = "ExactlyPairs".test
  override def schema: SchemaDescription = schemaDescription
  override  def setup(instance: DBInstance)(implicit ec: ExecutionContext): ConstrainedFuture[E, Unit] =
    using(instance){
      DBBuilder.buildDB("imdb/smallest")(instance)
    }

  override def test(instance: DBInstance)(index: TestIndex)(implicit ec: ExecutionContext): ConstrainedFuture[E, Set[(Person, Person)]] =
    for { res <- {
      implicit val inst = instance
      using(instance){
        findPairs((ActsIn --><-- ActsIn) * index.i)
      }
    }
          _ = logger.info("Length of exactlies = " + res.size)
    } yield res

  override def batchSize: TestIndex = 5.tests
  override def ignoreBackends: Set[String] = Set(lmdbOriginal)
}
