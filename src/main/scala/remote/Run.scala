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
      fastjoins.LMDB,
      // SQLDB,
      Vector[(String, DBBackend)](
    //    "SQL" -> SQLDB,
    //    "LMDB" -> original.LMDB,
        lmdbcse -> cse.LMDB,
        lmdbfast -> fast.LMDB,
        lmdbOriginal -> original.LMDB,

        postgres -> SQLDB
      )
    )

    tester.runTest(ExactlyTest)
    tester.runTest(ExactlySparse)
    tester.runTest(RawLookupLarge)
    tester.runTest(FindSinglesLarge)
    tester.runTest(RawLookup)
    tester.runTest(PathFindingTest)
    tester.runTest(JoinSpeed)
    tester.runTest(FindSingles)
    tester.runTest(Writes)
    tester.runTest(ConjunctionsAndDisjunctions)



    // dodgy tests

    // CSE/LMDB test for JoinSpeed (Massive joins)
    // todo: fix pathfinding
  }
}