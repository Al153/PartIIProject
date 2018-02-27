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


/**
  * Created by Al on 02/02/2018.
  *
  * Formerly find singles
  */
object UptoTest extends TestSpec[Set[(Person, Person)]] {
  override def testName: TestName = "Upto".test

  override def batchSize: TestIndex = 10.tests

  override def setup(d: DBInstance[_ <: E])(implicit ec: ExecutionContext): ConstrainedFuture[E, Unit] = {
    implicit val inst: DBInstance[_ <: E] = d

    using(d){
      DBBuilder.buildDB("imdb/large")
    }
  }


  override def test(d: DBInstance[_ <: E])(index: TestIndex)(implicit ec: ExecutionContext): ConstrainedFuture[E, Set[(Person, Person)]] =
    {
      implicit val inst = d
      readDefault(inst) {
        findPairs(((ActsIn -->(KevinBacon >> ActsIn))<-- ActsIn).*(0 --> index.i))
      }
    }

  override def schema: SchemaDescription = IMDBSchema.schemaDescription
  override def ignoreBackends: Set[String] = Set()
}
