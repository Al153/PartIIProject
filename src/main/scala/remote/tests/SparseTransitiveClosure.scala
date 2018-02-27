package remote.tests

import construction.ufc.UFCSchema.{Beat, LighterThan, Person, ShorterThan}
import core.user.containers.ConstrainedFuture
import core.user.dsl.E
import core.user.interfaces.DBInstance
import core.user.dsl._
import construction.ufc.{DBBuilder, UFCSchema}
import remote.util.{TestIndex, TestName, TestSpec}
import TestIndex._
import TestName._
import core.user.schema.SchemaDescription
import remote._

import scala.concurrent.ExecutionContext

object SparseTransitiveClosure extends TestSpec[Set[(Person, Person)]] {
  override def testName: TestName = "SparseTransitiveClosure".test

  override def batchSize: TestIndex = 10.tests

  override def setup[ThisE <: E](d: DBInstance[ThisE])(implicit R: HasRecovery[ThisE], ec: ExecutionContext): ConstrainedFuture[ThisE, Unit] =
    {
      implicit val instance = d
      using(d){
        DBBuilder.buildDB("ufc")
      }
    }

  override def test[ThisE <: E](d: DBInstance[ThisE])(index: TestIndex)(implicit R: HasRecovery[ThisE], ec: ExecutionContext): ConstrainedFuture[ThisE, Set[(Person, Person)]] = {
    implicit val instance = d
    using(d){
      findPairs(Beat & (ShorterThan.++ | LighterThan.++))
    }
  }

  override def schema: SchemaDescription = UFCSchema.schema

  override def ignoreBackends: Set[String] = Set(lmdbOriginal)
}