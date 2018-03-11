package remote.tests

import construction.imdb.IMDBSchema.{ActsIn, Person}
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
  * Created by Al on 02/02/2018.
  *
  * Larger verson of FindSingles. Only test postgres and FastJoins impl, since they are the fast ones
  * Repeats tests for better reliability
  */
object UptoLarge extends TestSpec[Set[(Person, Person)]] {
  override def testName: TestName = "UptoLarge".test

  override def batchSize: TestIndex = 100.tests

  override def setup[ThisE <: E](d: DBInstance[ThisE])(implicit R: HasRecovery[ThisE], ec: ExecutionContext): ConstrainedFuture[ThisE, Unit] = {
    implicit val inst: DBInstance[ThisE] = d

    using(d){
      DBBuilder.buildDB("imdb/large")
    }
  }


  override def test[ThisE <: E](d: DBInstance[ThisE])(index: TestIndex)(implicit R: HasRecovery[ThisE], ec: ExecutionContext): ConstrainedFuture[ThisE, Set[(Person, Person)]] =
    {
      implicit val inst = d
      readDefault(inst) {
        findPairs(((ActsIn -->(KevinBacon >> ActsIn))<-- ActsIn).*(0 --> (index.i % 10)))
      }
    }

  override def schema: SchemaDescription = IMDBSchema.schemaDescription
  override def ignoreBackends: Set[String] = Set(lmdbfast, lmdbcse, lmdbOriginal)
}
