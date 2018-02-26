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

    tester.runTest(ExactlyPairs)
    tester.runTest(ExactlySparse)
    tester.runTest(PathFindingTest)

    tester.runTest(RawLookupLarge)
    tester.runTest(UptoSparse)
    tester.runTest(RawLookup)
    tester.runTest(JoinSpeed)
    tester.runTest(UptoTest)
    tester.runTest(Writes)
    tester.runTest(SparseTransitiveClosure)
    tester.runTest(ExactlyTest)



    // dodgy tests

    // CSE/LMDB test for JoinSpeed (Massive joins)
    // todo: fix pathfinding
  }
}