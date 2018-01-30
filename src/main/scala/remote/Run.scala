package remote
import core.user.interfaces.DBBackend
import impl.memory.MemoryDB
import impl.{lmdb, lmdbfast}
import remote.tests.{ExactlyTest, PathFindingTest}

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

    tester.runTest(ExactlyTest)
    tester.runTest(PathFindingTest)
  }
}