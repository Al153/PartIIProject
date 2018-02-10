package remote
import core.user.interfaces.DBBackend
import impl.lmdb._
import impl.sql.SQLDB
import remote.tests._
import remote.util.RemoteTester

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds

object Run {
  def main(args: Array[String]): Unit = {
    val tester = new RemoteTester(
      // MemoryDB,
      cse.LMDB,
      // SQLDB,
      Vector[(String, DBBackend)](
    //    "SQL" -> SQLDB,
    //    "LMDB" -> original.LMDB,
        "LMDB Log Joins" -> logjoins.LMDB,
        "LMDBFast" -> fast.LMDB,
        "SQL" -> SQLDB
      )
    )


    tester.runTest(JoinSpeed)
    tester.runTest(FindSingles)
    tester.runTest(Writes)
    tester.runTest(ConjunctionsAndDisjunctions)
    tester.runTest(PathFindingTest)
    tester.runTest(RawLookup)
    tester.runTest(ExactlyTest)
  }
}