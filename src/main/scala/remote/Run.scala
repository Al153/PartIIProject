package remote
import core.backend.intermediate.FindSingle
import core.user.containers.ConstrainedFuture
import core.user.dsl.FindPairAble
import core.user.interfaces.{DBBackend, DBInstance}
import core.user.schema.SchemaObject
import core.user.dsl._
import impl.lmdb._
import impl.memory.MemoryDB
import impl.sql.SQLDB
import remote.tests._
import remote.util.RemoteTester
import impl.lmdb.original

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds
import scalaz.Monad

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

    tester.runTest(FindSingles)
    tester.runTest(Writes)
    tester.runTest(ConjunctionsAndDisjunctions)
    tester.runTest(ExactlyTest)
    tester.runTest(PathFindingTest)
    tester.runTest(RawLookup)
  }
}