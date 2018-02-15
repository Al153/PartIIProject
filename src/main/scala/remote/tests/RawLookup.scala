package remote.tests

import construction.imdb.IMDBSchema._
import core.user.containers.ConstrainedFuture
import core.user.dsl._
import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription
import construction.imdb.{DBBuilder, IMDBSchema}
import remote.util.{TestIndex, TestName, TestSpec}
import TestIndex._
import TestName._

import scala.concurrent.ExecutionContext


object RawLookup extends TestSpec[Set[(Person, Movie)]] {
  override def testName: TestName = "RawLookupPerformance".test

  override def batchSize: TestIndex = 10.tests // todo: increase for long tests

  override def setup(d: DBInstance)(implicit ec: ExecutionContext): ConstrainedFuture[E, Unit] =
    using(d){
      DBBuilder.buildDB("imdb/medium")(d)
    }

  override def test(d: DBInstance)
                   (index: TestIndex)
                   (implicit ec: ExecutionContext): ConstrainedFuture[E, Set[(Person, Movie)]] = {
    implicit val instance = d
    val coactor = ActsIn --><-- ActsIn
    val sameDirector = Directed.rev -->--> Directed
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
}