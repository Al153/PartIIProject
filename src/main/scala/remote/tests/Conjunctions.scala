package remote.tests

import construction.imdb.IMDBSchema.{ActsIn, Person}
import construction.imdb.{DBBuilder, IMDBSchema}
import core.user.containers.ConstrainedFuture
import core.user.dsl._
import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription
import remote.lmdbOriginal
import remote.util.TestIndex._
import remote.util.TestName._
import remote.util.{TestIndex, TestName, TestSpec}

import scala.concurrent.ExecutionContext

object Conjunctions extends TestSpec[Set[(Person, Person)]]{
  override def testName: TestName = "Conjunctions".test

  override def batchSize: TestIndex = 12.tests

  override def setup[ThisE <: E](d: DBInstance[ThisE])(implicit R: HasRecovery[ThisE], ec: ExecutionContext): ConstrainedFuture[ThisE, Unit] =
    using(d){
      DBBuilder.buildDB("imdb/large")(d)
    }

  private def actsWith(a: Person) =
    (ActsIn--><-- ActsIn) ->>- a

  override def test[ThisE <: E](d: DBInstance[ThisE])(index: TestIndex)(implicit R: HasRecovery[ThisE],ec: ExecutionContext): ConstrainedFuture[ThisE, Set[(Person, Person)]] = {
    implicit val inst = d
    readDefault(d) {
      (index.i % 3) match {
        case 0 => findPairs(actsWith(KevinBacon))
        case 1 => findPairs(actsWith(KevinBacon) & actsWith(TomCruise))
        case _ => findPairs(actsWith(KevinBacon) & actsWith(TomCruise) & actsWith(TomHanks))
      }
    }
  }

  override def schema: SchemaDescription = IMDBSchema.schemaDescription

  override def ignoreBackends: Set[String] = Set()
}