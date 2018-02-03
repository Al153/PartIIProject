package remote
import core.user.interfaces.DBBackend
import impl.memory.MemoryDB
import impl.sql.SQLDB
import impl.{lmdb, lmdbfast}
import remote.tests._

import scala.concurrent.ExecutionContext.Implicits.global

object Run {
  def main(args: Array[String]): Unit = {
    val tester = new RemoteTester(
      MemoryDB,
      Vector[(String, DBBackend)](
        "SQL" -> SQLDB,
        "LMDB" -> lmdb.LMDB,
        "LMDBFast" -> lmdbfast.LMDB
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