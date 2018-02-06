package remote
import core.user.interfaces.DBBackend
import impl.lmdb.fast.LMDB
import impl.memory.MemoryDB
import impl.sql.SQLDB
import remote.tests._
import remote.util.RemoteTester
import impl.lmdb.original

import scala.concurrent.ExecutionContext.Implicits.global

object Run {
  def main(args: Array[String]): Unit = {
    val tester = new RemoteTester(
      MemoryDB,
      Vector[(String, DBBackend)](
        "SQL" -> SQLDB,
        "LMDB" -> original.LMDB,
        "LMDBFast" -> LMDB
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