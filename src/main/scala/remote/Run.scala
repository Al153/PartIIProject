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
      Some(lmdbcse -> cse.LMDB),
      Some(lmdbfast -> fast.LMDB),
      Some(lmdbOriginal -> original.LMDB),
      Some(postgres -> SQLDB),
      None,
      None
    )


    // tests that need to be redone
    tester.runTest(Conjunctions)
    tester.runTest(Disjunctions)

    // tests that trigger running out of file size.

    tester.runTest(ExactlyPairs)
    tester.runTest(ExactlyTest)
    tester.runTest(Redundancy)

    tester.runTest(UptoSparse)
    tester.runTest(ExactlySparse)

    tester.runTest(SingleShortestPath)

    tester.runTest(SparseTransitiveClosure)

    tester.runTest(RawLookupLarge)

    tester.runTest(RawLookup)
    tester.runTest(JoinSpeed)
    tester.runTest(UptoTest)
    tester.runTest(Writes)

    // slowest test
    tester.runTest(PathFindingTest)



  }
}