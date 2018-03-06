package remote.tests

import construction.imdb.IMDBSchema.{ActsIn, Person}
import construction.imdb.{DBBuilder, IMDBSchema}
import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, HasRecovery, findPairs, readDefault, using}
import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription
import remote.util.TestIndex._
import remote.util.TestName._
import remote.util.{TestIndex, TestName, TestSpec}
import remote._

import scala.concurrent.ExecutionContext


/**
  * Differential test to see join speed
  */
object JoinSpeed extends TestSpec[Set[(Person, Person)]] {
  override def testName: TestName = "JoinSpeed".test

  override def batchSize: TestIndex = 300.tests

  override def setup[ThisE <: E](instance: DBInstance[ThisE])(implicit R: HasRecovery[ThisE], ec: ExecutionContext): ConstrainedFuture[ThisE, Unit] =
    using(instance) {
      DBBuilder.buildDB("imdb/medium_sparse")(instance)
    }

  override def test[ThisE <: E](d: DBInstance[ThisE])(index: TestIndex)(implicit R: HasRecovery[ThisE], ec: ExecutionContext): ConstrainedFuture[ThisE, Set[(Person, Person)]] = {
    implicit val inst = d
    readDefault(d) {
      (index.i % 2) match {
        case 0 => findPairs((ActsIn --><-- ActsIn) & (ActsIn --><-- ActsIn))
        case _ => findPairs((ActsIn --><-- ActsIn) -->--> (ActsIn --><-- ActsIn))
      }
    }
  }

  override def schema: SchemaDescription = IMDBSchema.schemaDescription

  override def ignoreBackends: Set[String] = Set(lmdbOriginal)
}