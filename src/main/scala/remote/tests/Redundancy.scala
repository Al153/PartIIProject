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
  * FindSingle queries with lots of redundancy
  */

object Redundancy extends TestSpec[Set[Person]]{
  override def testName: TestName = "Redundancy".test

  override def batchSize: TestIndex = 12.tests

  override def setup[ThisE <: E](d: DBInstance[ThisE])(implicit R: HasRecovery[ThisE], ec: ExecutionContext): ConstrainedFuture[ThisE, Unit] =
    using(d){
      DBBuilder.buildDB("imdb/large")(d)
    }

  private def coactor = ActsIn --><-- ActsIn

  override def test[ThisE <: E](d: DBInstance[ThisE])(index: TestIndex)(implicit R: HasRecovery[ThisE],ec: ExecutionContext): ConstrainedFuture[ThisE, Set[Person]] = {
    implicit val inst = d
    readDefault(d) {
      (index.i % 3) match {
        case 0 => find(KevinBacon >> coactor)
        case 1 => find(KevinBacon >> (coactor -->--> (coactor -->--> coactor)))
        case _ => find(KevinBacon >> (coactor -->--> (coactor -->--> (coactor -->--> coactor))))
      }
    }
  }

  override def schema: SchemaDescription = IMDBSchema.schemaDescription

  override def ignoreBackends: Set[String] = Set(lmdbOriginal)
}