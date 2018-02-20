package remote.tests

import construction.imdb.DBBuilder
import construction.imdb.IMDBSchema.{ActsIn, Person, schemaDescription}
import core.user.containers.ConstrainedFuture
import core.user.dsl._
import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription
import remote.util.TestIndex._
import remote.util.{TestIndex, TestSpec}
import remote.util.TestName._

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

  override def batchSize: TestIndex = 5.tests
  override def ignoreBackends: Set[String] = Set()
}
