package remote.tests

import construction.imdb.IMDBSchema.{ActsIn, Person}
import construction.imdb.{DBBuilder, IMDBSchema}
import core.user.containers.ConstrainedFuture
import core.user.dsl._
import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription
import remote.util.{TestIndex, TestName, TestSpec}
import TestIndex._
import TestName._
import scala.concurrent.ExecutionContext
import remote._


/**
  * Created by Al on 02/02/2018.
  *
  * Formerly find singles
  */
object UptoTest extends TestSpec[Set[(Person, Person)]] {
  override def testName: TestName = "Upto".test

  override def batchSize: TestIndex = 10.tests

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
        findPairs(((ActsIn -->(KevinBacon >> ActsIn))<-- ActsIn).*(0 --> index.i))
      }
    }

  override def schema: SchemaDescription = IMDBSchema.schemaDescription
  override def ignoreBackends: Set[String] = Set(lmdbOriginal)
}
