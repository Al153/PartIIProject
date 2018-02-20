package remote.tests

import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, findPairs, readDefault, using}
import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription
import remote.util.{TestIndex, TestName, TestSpec}
import TestIndex._
import TestName._
import construction.imdb.{DBBuilder, IMDBSchema}
import construction.imdb.IMDBSchema.{ActsIn, Directed, Person}

import scala.concurrent.ExecutionContext


/**
  * Differential test to see join speed
  */
object JoinSpeed extends TestSpec[Set[(Person, Person)]] {
  override def testName: TestName = "JoinSpeed".test

  override def batchSize: TestIndex = 30.tests

  override def setup(instance: DBInstance)(implicit ec: ExecutionContext): ConstrainedFuture[E, Unit] =
    using(instance){
      DBBuilder.buildDB("imdb/medium_sparse")(instance)
    }

  override def test(d: DBInstance)(index: TestIndex)
                   (implicit ec: ExecutionContext)
  : ConstrainedFuture[E, Set[(Person, Person)]] = {
    implicit val inst = d
    readDefault(d) {
      (index.i % 2) match {
        case 0 => findPairs((ActsIn --><-- ActsIn) & (ActsIn --><-- ActsIn))
        case _ => findPairs((ActsIn --><-- ActsIn) -->--> (ActsIn --><-- ActsIn))
      }
    }
  }

  override def schema: SchemaDescription = IMDBSchema.schemaDescription
  override def ignoreBackends: Set[String] = Set()
}