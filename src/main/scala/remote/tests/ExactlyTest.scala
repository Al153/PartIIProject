package remote.tests

import construction.imdb.DBBuilder
import construction.imdb.IMDBSchema.{ActsIn, Person, schemaDescription}
import core.user.containers.ConstrainedFuture
import core.user.dsl._
import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription
import remote.util.TestIndex._
import remote.util.{TestIndex, TestSpec}
import remote.util.TestName._
import remote._

import scala.concurrent.ExecutionContext



/**
  * Created by Al on 30/01/2018.
  */
object ExactlyTest extends TestSpec[Set[Person]] {
  override def testName = "Exactly".test
  override def schema: SchemaDescription = schemaDescription
  override  def setup[ThisE <: E](instance: DBInstance[ThisE])(implicit R: HasRecovery[ThisE], ec: ExecutionContext): ConstrainedFuture[ThisE, Unit] =
    using(instance){
      DBBuilder.buildDB("imdb/small")(instance)
    }

  override def test[ThisE <: E](instance: DBInstance[ThisE])(index: TestIndex)(implicit R: HasRecovery[ThisE], ec: ExecutionContext): ConstrainedFuture[ThisE, Set[Person]] =
  for { res <- {
      implicit val inst = instance
      using(instance){
        find(TomHanks >> (((ActsIn -->(KevinBacon >> ActsIn))<-- ActsIn) * (index.i % 10)))
      }
  }
    _ = logger.info("Length of exactlies = " + res.size)
  } yield res

  override def batchSize: TestIndex = 50.tests
  override def ignoreBackends: Set[String] = Set(lmdbOriginal, postgres)
}
