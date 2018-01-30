package remote

import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, Empty}
import core.user.interfaces.{DBBackend, DBInstance}
import core.user.schema.SchemaDescription
import core.utils.EitherOps
import org.slf4j.Logger

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scalaz._

/**
  * Created by Al on 28/01/2018.
  *
  * Class to run remote tests and time them
  */
class RemoteTester(
                    referenceImplementation: DBBackend,
                    testImplementations: Vector[(String, DBBackend)]
                  )(implicit logger: Logger, ec: ExecutionContext) {


  def runTest[A](
                  testSpec: TestSpec,
                  setup: DBInstance => ConstrainedFuture[E,Unit],
                  test: DBInstance => TestIndex =>  ConstrainedFuture[E, A],
                  schema: SchemaDescription
                ): Unit = {
    val runningTests = (for {
      ref <- referenceImplementation.open(Empty, schema)
      instances <- EitherOps.sequence(testImplementations.map{case (n, instance) => instance.open(Empty, schema).map(n -> _)})
    } yield (ref, instances)).fold(
      {e =>
        logger.info("Error thrown " + e)
        ConstrainedFuture.point(logError(e))(CaughtError.apply)},
      {case (ref, i) => runBatches(testSpec, setup, test, ref, i)}
    )
    Await.result(runningTests.run, (60*60*24).seconds) match {
      case \/-(()) => logger.info("[Done]")
      case -\/(e) => errorThrowable(e)
    }
  }

  def doSetup(
               setup: (DBInstance) => ConstrainedFuture[E, Unit],
               instance: DBInstance,
               name: String
             ): ConstrainedFuture[E, Unit] = for {
    _ <- setup(instance)
    _ = logger.info(s"[Setup][Done]: Instance = $name")
  } yield logger.info(s"[Setup][Done]: Instance = $name")

  def runBatches[A](spec: TestSpec, setup: DBInstance => ConstrainedFuture[E, Unit], test: DBInstance => TestIndex =>  ConstrainedFuture[E, A], reference: DBInstance, toTest: Vector[(String, DBInstance)]): ConstrainedFuture[E, Unit] = {
    logger.info("Starting Setup")
    for {
      _ <- doSetup(setup, reference, "reference")
      _ = logger.info("[Starting reference batch]")
      refValues <- runReferenceBatch(spec.testName, reference, test, spec.batchSize)
      _ = logger.info("[Done reference batch]")
      _ <- sequence(toTest) {
        case (name, instance) =>
          logger.info(s"[Starting test for $name] ")
          for {
            _ <- doSetup(setup, instance, name)
            _ = logger.info(s"[Done setup for $name] ")
            _ <- runTestBatch(spec.testName, instance, name, test, refValues, spec.batchSize)
            _ = logger.info(s"[Run test for $name] ")

          } yield ()
      }
    } yield ()
  }

  def runBatch[A](
                   testName: TestName,
                   impl: DBInstance,
                   instanceName: String,
                   test: DBInstance => TestIndex =>  ConstrainedFuture[E, A],
                   n: TestIndex): ConstrainedFuture[E, Map[TestInstance, TimeResult[A]]] =
    sequence(TestInstance(testName, instanceName)(n)) {
      i =>
        logger.info(s"Running $i" )
        timeConstrainedFuture(test(impl))(i)
    }

  def runReferenceBatch[A](
                            testName: TestName,
                            impl: DBInstance,
                            test: DBInstance => TestIndex => ConstrainedFuture[E, A],
                            n: TestIndex
                          ): ConstrainedFuture[E, Map[TestIndex, A]] =
    for {
      instanceToTimedResult <- runBatch(testName, impl, "ReferenceInstance", test, n)
      timeResults = instanceToTimedResult.map {case (t, res) => t.testIndex -> res}
      referenceResults = instanceToTimedResult.map {case (t, res) => t.testIndex -> res.a}
      _ = timeResults.values.map(logTimeResult(_, success = true))
      _ = logBatchResult(BatchedTimedResults(instanceToTimedResult.values.toSeq))
    } yield referenceResults


  def logTimeResult[A](ta: TimeResult[A], success: Boolean): A = {
    if (success){
      logger.info(s"[Result][Success][${ta.instance.testBackend}]:${ta.instance}:time=${ns(ta.ns)}")
    } else {
      logger.info(s"[Result][Failure][${ta.instance.testBackend}]:${ta.instance}:time=${ns(ta.ns)}")
    }
    ta.a
  }

  def logBatchResult[A](tas: BatchedTimedResults[A]): Map[TestInstance, A] = {
    logger.info(s"[Result][Batch]:test=${tas.testName.name}:impl=${tas.backend}:testcount=${tas.length}:totalTime=${ns(tas.fullTime)}")
    tas.tas.map {r => r.instance -> r.a}.toMap
  }

  def runTestBatch[A](
                       testName: TestName,
                       impl: DBInstance,
                       toTest: String,
                       test: DBInstance => TestIndex => ConstrainedFuture[E, A],
                       expectedResults: Map[TestIndex, A],
                       n: TestIndex
                     ): ConstrainedFuture[E, Unit] = {
    for {
      instanceToTimedResult <- runBatch(testName, impl, toTest, test, n)
      timeResults = instanceToTimedResult.map {case (t, res) => t.testIndex -> res}
      testResults = instanceToTimedResult.map {case (t, res) => t.testIndex -> res.a}
      isSuccess = testResults == expectedResults
      _ = timeResults.values.map(logTimeResult(_, success = isSuccess))
      _ = logBatchResult(BatchedTimedResults(instanceToTimedResult.values.toSeq))
    } yield ()
  }

  def logError(e: E): Unit = {
    logger.error("Hit an error when setting up instances. Halting tests")
    logger.error(e.toString)
  }

  def sequence[A, B](in: TraversableOnce[A])(f: A => ConstrainedFuture[E, B]): ConstrainedFuture[E, Map[A, B]] =
    in.foldLeft(ConstrainedFuture.point(Map.empty[A, B])(CaughtError(_): E)){
      case (or, a) =>
        for {
          r <- or
          b <- f(a)
        } yield r + (a -> b)
    }

  def timeConstrainedFuture[R](block: TestIndex => ConstrainedFuture[E, R])(testInstance: TestInstance): ConstrainedFuture[E, TimeResult[R]] = {
    val t0 = System.nanoTime()
    for {
      r <- block(testInstance.testIndex)
      t1 = System.nanoTime()
    } yield TimeResult(testInstance, t1 - t0,  r)
  }

}

case class CaughtError(e: Throwable) extends E









