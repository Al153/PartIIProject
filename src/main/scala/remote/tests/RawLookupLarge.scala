package remote.tests

import construction.imdb.IMDBSchema._
import construction.imdb.{DBBuilder, IMDBSchema}
import core.user.containers.ConstrainedFuture
import core.user.dsl._
import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription
import remote.util.TestIndex._
import remote.util.TestName._
import remote.util.{TestIndex, TestName, TestSpec}
import remote._

import scala.concurrent.ExecutionContext

/**
  * Copy of raw lookup performance, skips SQL because SQL runs out of memory
  */

object RawLookupLarge extends TestSpec[Set[(Person, Movie)]] {
  override def testName: TestName = "RawLookupPerformanceLarge".test

  override def batchSize: TestIndex = 100.tests // todo: increase for long tests

  override def setup[ThisE <: E](d: DBInstance[ThisE])(implicit R: HasRecovery[ThisE], ec: ExecutionContext): ConstrainedFuture[ThisE, Unit] =
    using(d){
      DBBuilder.buildDB("imdb/medium")(d)
    }

  override def test[ThisE <: E](d: DBInstance[ThisE])(index: TestIndex)(implicit R: HasRecovery[ThisE], ec: ExecutionContext): ConstrainedFuture[ThisE, Set[(Person, Movie)]] = {
    implicit val instance = d
    val coactor = ActsIn --><-- ActsIn
    val sameDirector = Directed <----> Directed
    (index.i % 5) match {
      case 0 =>
        readDefault(d){
          findPairs(ActsIn)
        }
      case 1 =>
        readDefault(d){
          findPairs(Directed)
        }
      case 2 =>
        readDefault(d) {
          findPairs(coactor -->--> ActsIn)
        }
      case 3 =>
        readDefault(d) {
          findPairs(ActsIn & Directed)
        }
      case _ =>
        readDefault(d){
          findPairs(ActsIn -->--> sameDirector)
        }
    }
  }

  override def schema: SchemaDescription = IMDBSchema.schemaDescription
  override def ignoreBackends: Set[String] = Set(postgres, lmdbOriginal)
}