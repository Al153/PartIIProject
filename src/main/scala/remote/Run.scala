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
        "SQL" -> SQLDB,
        "LMDBFast" -> fast.LMDB
      )
    )

    tester.runTest(ExactlyTest)
    tester.runTest(JoinSpeed)
    tester.runTest(FindSingles)
    tester.runTest(Writes)
    tester.runTest(ConjunctionsAndDisjunctions)
    tester.runTest(PathFindingTest)
    tester.runTest(RawLookup)
  }

  val a = new Array[Int](5)

  a(3) = 2
  println(a(3) )
}