package remote
import core.user.interfaces.DBBackend
import impl.lmdb._
import impl.memory.MemoryDB
import impl.sql.SQLDB
import remote.tests._
import remote.util.RemoteTester
import impl.lmdb.original

import scala.concurrent.ExecutionContext.Implicits.global

object Run {
  def main(args: Array[String]): Unit = {
    val tester = new RemoteTester(
      // MemoryDB,
      SQLDB,
      Vector[(String, DBBackend)](
    //    "SQL" -> SQLDB,
    //    "LMDB" -> original.LMDB,
        "LMDBCSE" -> cse.LMDB,
        "LMDBFast" -> fast.LMDB
      )
    )

    tester.runTest(FindSingles)
    tester.runTest(Writes)
    tester.runTest(ConjunctionsAndDisjunctions)
    tester.runTest(ExactlyTest)
    tester.runTest(PathFindingTest)
    tester.runTest(RawLookup)
  }
}