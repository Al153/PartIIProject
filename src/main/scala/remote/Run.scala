package remote
import TestIndex._
import construction.imdb.DBBuilder
import construction.imdb.IMDBSchema._
import core.user.containers.ConstrainedFuture
import core.user.dsl._
import core.user.interfaces.{DBBackend, DBInstance}
import impl.{lmdb, lmdbfast}
import impl.memory.MemoryDB

import scala.concurrent.ExecutionContext.Implicits.global

object Run {
  def main(args: Array[String]): Unit = {
    val tester = new RemoteTester(
      new TestSpec {
        override def batchSize: TestIndex = 5.tests

        override def testImplementations: Vector[(String, DBBackend)] =
          Vector(
            "LMDB" -> lmdb.LMDB,
            "LMDBFast" -> lmdbfast.LMDB
          )

        override def referenceImplementation: DBBackend = MemoryDB
      }
    )

    tester.runTest(
      setup,
      test,
      schemaDescription
    )
  }

  val KevinBacon = Person("Kevin Bacon")

  private def setup(instance: DBInstance): ConstrainedFuture[E, Unit] =
    using(instance){
      DBBuilder.buildDB("imdb/smallest")(instance)
    }

  private def test(instance: DBInstance): TestIndex => ConstrainedFuture[E, Set[Person]] = {
    index =>
      implicit val inst = instance
      using(instance){
        find(KevinBacon >> ((ActsIn --><-- ActsIn) * index.i))
      }
  }

}