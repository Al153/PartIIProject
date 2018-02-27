package remote.tests

import construction.imdb.{DBBuilder, IMDBSchema}
import core.user.containers.ConstrainedFuture
import core.user.dsl._
import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription
import remote.util.{TestIndex, TestName, TestSpec}
import TestIndex._
import TestName._
import remote._

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 02/02/2018.
  */
object Writes extends TestSpec[Unit]{
  override def testName: TestName = "WriteSpeed".test

  override def batchSize: TestIndex = 7.tests

  override def setup(d: DBInstance[_ <: E])(implicit ec: ExecutionContext): ConstrainedFuture[E, Unit] =
    ConstrainedFuture.point(())


  override def test(d: DBInstance[_ <: E])(index: TestIndex)(implicit ec: ExecutionContext): ConstrainedFuture[E, Unit] = {
    implicit val inst: DBInstance[_ <: E] = d
    readDefault(d){
      index.i match {
        case 1 => DBBuilder.buildDB("imdb/smallest")
        case 2 => DBBuilder.buildDB("imdb/smallest")
        case 3 => DBBuilder.buildDB("imdb/smallest")
        case 4 => DBBuilder.buildDB("imdb/smallest")
        case 5 => DBBuilder.buildDB("imdb/smallest")
        case 6 => DBBuilder.buildDB("imdb/smallest")
        case _ => DBBuilder.buildDB("imdb/smallest")
      }
    }
  }


  override def schema: SchemaDescription = IMDBSchema.schemaDescription

  // these have the same write implementations as lmdb fastjoins, so can ignore
  override def ignoreBackends: Set[String] = Set(lmdbOriginal, lmdbcse, lmdbfast)
}
