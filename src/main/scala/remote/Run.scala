package remote
import impl.lmdb._
import impl.sql.SQLDB
import remote.tests._
import remote.util.RemoteTester

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds

object Run {
  def main(args: Array[String]): Unit = {
    val tester = new RemoteTester(
      referenceImplementation = fastjoins.LMDB,
      Some(postgres -> SQLDB),
      Some(lmdbcse -> cse.LMDB),
      Some(lmdbfast -> fast.LMDB),
      Some(lmdbOriginal -> original.LMDB),
      None,
      None
    )

    tester.runTest(Redundancy)
    tester.runTest(PathFindingTest)
    tester.runTest(Conjunctions)
    tester.runTest(Disjunctions)
    tester.runTest(ExactlySparse)
    tester.runTest(ExactlyPairs)
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