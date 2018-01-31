package remote
import core.user.interfaces.DBBackend
import impl.memory.MemoryDB
import impl.{lmdb, lmdbfast}
import remote.tests.{ConjunctionsAndDisjunctions, ExactlyTest, PathFindingTest, RawLookup}

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

    tester.runTest(RawLookup)
    tester.runTest(ConjunctionsAndDisjunctions)
    tester.runTest(ExactlyTest)
    tester.runTest(PathFindingTest)
  }
}