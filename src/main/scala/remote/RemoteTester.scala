package remote

import core.user.containers.Operation
import core.user.dsl.{E, Empty}
import core.user.interfaces.{DBBackend, DBInstance}
import core.user.schema.SchemaDescription
import core.utils.EitherOps

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.HashMap

/**
  * Created by Al on 28/01/2018.
  *
  * Class to run remote tests and time them
  */
class RemoteTester(spec: TestSpec) {


  def runTest[A](
                  setup: DBInstance => Operation[E,Unit],
                  test: DBInstance => Int =>  Operation[E, A],
                  schema: SchemaDescription
                ): Unit = {
    (for {
      ref <- spec.referenceImplementation.open(Empty, schema)
      instances <- EitherOps.sequence(spec.testImplementations.map(_.open(Empty, schema)))
    } yield (ref, instances)).fold(
      logError,
      {case (ref, i) => runBatches(test, ref, i)}
    )
  }

  def runBatches[A](test: DBInstance => Int =>  Operation[E, A], reference: DBInstance, toTest: Vector[DBInstance]): Unit = {
    logger.info("Finished Setup")

    for (batchIndex <- 1 until spec.batches) {
      for {
        refValue <- runBatch(reference, test, spec.perBatch)
        // todo: properly sequence
      } yield ???

    }
  }

  def runBatch[A](impl: DBInstance, test: DBInstance => Int =>  Operation[E, A], n: Int): Operation[E, Map[Int, A]] =
    sequence(1 until n) {
      test(impl)
    }

  def logError(e: E): Unit = {
    logger.error("Hit an error when setting up instances. Halting tests")
    logger.error(e.toString)
  }

  def sequence[A, B](
                         in: TraversableOnce[A]
                       )(
    f: A => Operation[E, B]
  ): Operation[E, Map[A, B]] =
    in.foldLeft(Operation.point(Map.empty[A, B], CaughtError(_): E)){
      case (or, a) =>
        for {
          r <- or
          b <- f(a)
        } yield r + (a -> b)
    }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

}

case class CaughtError(e: Throwable) extends E

case class TestInstance(testBackend: Int, testIndex: Int)

trait TestSpec {
  def batches: Int
  def referenceImplementation: DBBackend
  def testImplementations: Vector[DBBackend]
}
