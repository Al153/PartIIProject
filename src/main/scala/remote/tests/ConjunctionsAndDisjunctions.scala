package remote.tests

import construction.ufc.UFCSchema.{Beat, LighterThan, Person, ShorterThan}
import core.user.containers.ConstrainedFuture
import core.user.dsl.E
import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription
import remote.{TestIndex, TestName, TestSpec}
import TestName._
import TestIndex._
import core.user.dsl._
import construction.ufc.{DBBuilder, UFCSchema}

import scala.concurrent.ExecutionContext

object ConjunctionsAndDisjunctions extends TestSpec[Set[(Person, Person)]] {
  override def testName: TestName = "ConjunctionsAndDisjunctions".test

  override def batchSize: TestIndex = 10.tests

  override def setup(d: DBInstance)(implicit ec: ExecutionContext): ConstrainedFuture[E, Unit] =
    {
      implicit val instance = d
      using(d){
        DBBuilder.buildDB("ufc")
      }
    }

  override def test(
                     d: DBInstance
                   )(
    index: TestIndex
  )(implicit ec: ExecutionContext): ConstrainedFuture[E, Set[(Person, Person)]] = {
    implicit val instance = d
    using(d){
      findPairs(Beat & (ShorterThan.++ | LighterThan.++))
    }
  }

  override def schema: SchemaDescription = UFCSchema.schema
}