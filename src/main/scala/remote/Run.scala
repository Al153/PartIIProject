package remote
import construction.imdb.DBBuilder
import construction.imdb.IMDBSchema._
import core.user.containers.{ConstrainedFuture, Path}
import core.user.dsl._
import core.user.interfaces.{DBBackend, DBInstance}
import impl.memory.MemoryDB
import impl.{lmdb, lmdbfast}

import scala.concurrent.ExecutionContext.Implicits.global

object Run {
  def main(args: Array[String]): Unit = {
    val tester = new RemoteTester(
      MemoryDB,
      Vector[(String, DBBackend)](
        "LMDB" -> lmdb.LMDB,
        "LMDBFast" -> lmdbfast.LMDB
      )
    )

    testExactly(tester)
    pathFindingTest(tester)
  }

  def testExactly(tester: RemoteTester): Unit =
    tester.runTest(
      TestSpec("Exactly", 10),
      exactlySetup,
      exactlyTest,
      schemaDescription
    )

  private val KevinBacon = Person("Kevin Bacon")

  private def exactlySetup(instance: DBInstance): ConstrainedFuture[E, Unit] =
    using(instance){
      DBBuilder.buildDB("imdb/smallest")(instance)
    }

  private def exactlyTest(instance: DBInstance): TestIndex => ConstrainedFuture[E, Set[Person]] = {
    index =>
      implicit val inst = instance
      using(instance){
        find(KevinBacon >> ((ActsIn --><-- ActsIn) * index.i))
      }
  }


  private def pathFindingTest(tester: RemoteTester): Unit = {
    def setup(instance: DBInstance): ConstrainedFuture[E, Unit] =
      for {
        v0 <- instance.getDefaultView
        _ <- writeToView(instance, v0){
          DBBuilder.buildDB("imdb/smallest")(instance)
        }

        _ <- writeToView(instance, v0){
          DBBuilder.buildDB("imdb/small_sparse")(instance)
        }

        _ <- writeToView(instance, v0){
          DBBuilder.buildDB("imdb/small")(instance)
        }

        _ <- writeToView(instance, v0){
          DBBuilder.buildDB("imdb/medium_sparse")(instance)
        }

        _ <- writeToView(instance, v0){
          DBBuilder.buildDB("imdb/medium")(instance)
        }

        _ <- writeToView(instance, v0){
          DBBuilder.buildDB("imdb/large")(instance)
        }
      } yield ()


    def test(instance: DBInstance): (TestIndex) => ConstrainedFuture[E, Set[Path[Person]]] = {
      index: TestIndex =>
        implicit val inst = instance
        for {
          views <- instance.getViews
          v = views.toVector.apply(index.i)
          r <- usingView(instance, v){
            allShortestPaths(KevinBacon, (ActsIn --><-- ActsIn) * index.i)
          }
        } yield r
    }

    tester.runTest(
      TestSpec("Pathfinding", 6),
      setup,
      test,
      schemaDescription
    )
  }

}