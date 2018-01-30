package remote.tests

import construction.imdb.DBBuilder
import construction.imdb.IMDBSchema.{ActsIn, Person, schemaDescription}
import core.user.containers.ConstrainedFuture
import core.user.dsl._
import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription
import remote.TestIndex._
import remote.{TestIndex, TestName, TestSpec}
import TestName._

import scala.concurrent.ExecutionContext



/**
  * Created by Al on 30/01/2018.
  */
object ExactlyTest extends TestSpec[Set[Person]] {
  override def testName = "Exactly".test
  override def schema: SchemaDescription = schemaDescription
  override  def setup(instance: DBInstance)(implicit ec: ExecutionContext): ConstrainedFuture[E, Unit] =
    using(instance){
      DBBuilder.buildDB("imdb/smallest")(instance)
    }

  override def test(instance: DBInstance)(index: TestIndex)(implicit ec: ExecutionContext): ConstrainedFuture[E, Set[Person]] = {
      implicit val inst = instance
      using(instance){
        find(KevinBacon >> ((ActsIn --><-- ActsIn) * index.i))
      }
  }

  override def batchSize: TestIndex = 10.tests
}
