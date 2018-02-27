package remote.util

import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, Empty, HasRecovery}
import core.user.interfaces.{DBBackend, DBInstance}
import core.utils._
import remote.ns
import core.user.dsl.E.ERecovery

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scalaz._

/**
  * Created by Al on 28/01/2018.
  *
  * Class to run remote tests and time them
  */
class RemoteTester[ER <: E, E1 <: E, E2 <: E, E3 <: E, E4 <: E, E5 <: E, E6 <: E](
                    referenceImplementation: DBBackend[ER],
                    imp1: Option[(String, DBBackend[E1])],
                    imp2: Option[(String, DBBackend[E2])],
                    imp3: Option[(String, DBBackend[E3])],
                    imp4: Option[(String, DBBackend[E4])],
                    imp5: Option[(String, DBBackend[E5])],
                    imp6: Option[(String, DBBackend[E6])])(implicit ec: ExecutionContext,
                    RR: HasRecovery[ER],
                    R1: HasRecovery[E1],
                    R2: HasRecovery[E2],
                    R3: HasRecovery[E3],
                    R4: HasRecovery[E4],
                    R5: HasRecovery[E5],
                    R6: HasRecovery[E6]
) extends Logged {

  def runTest[A](testSpec: TestSpec[A]): Unit = {
    val runningTests = (for {
      ref <- referenceImplementation.open(Empty, testSpec.schema).leftMap(e => e: E)
      i1 <- imp1.mapSwitch{case (n, db) => db.open(Empty, testSpec.schema).map(n ->_).leftMap(e => e: E)}
      i2 <- imp2.mapSwitch{case (n, db) => db.open(Empty, testSpec.schema).map(n ->_).leftMap(e => e: E)}
      i3 <- imp3.mapSwitch{case (n, db) => db.open(Empty, testSpec.schema).map(n ->_).leftMap(e => e: E)}
      i4 <- imp4.mapSwitch{case (n, db) => db.open(Empty, testSpec.schema).map(n ->_).leftMap(e => e: E)}
      i5 <- imp5.mapSwitch{case (n, db) => db.open(Empty, testSpec.schema).map(n ->_).leftMap(e => e: E)}
      i6 <- imp6.mapSwitch{case (n, db) => db.open(Empty, testSpec.schema).map(n ->_).leftMap(e => e: E)}
    } yield (ref, i1, i2, i3, i4, i5, i6)).fold(
      {e =>
        logger.info("Error thrown " + e)
        ConstrainedFuture.point(logError(e))(ec, ERecovery)},
      {case (ref, i1, i2, i3, i4, i5, i6) => runBatches(testSpec, ref, i1, i2, i3, i4, i5, i6)}
    )
    Await.result(runningTests.run, (60*60*24*365).seconds) match {
      case \/-(()) => logger.info("[Done]")
      case -\/(e) => logger.info("[Error]" + e.toString)
    }
  }

  private def doSetup[ThisE <: E](
               setup: (DBInstance[ThisE]) => ConstrainedFuture[ThisE, Unit],
               instance: DBInstance[ThisE],
               name: String
             ): ConstrainedFuture[ThisE, Unit] = for {
    _ <- setup(instance)
    _ = logger.info(s"[Setup][Done]: Instance = $name")
  } yield logger.info(s"[Setup][Done]: Instance = $name")

  private def runOptionTest[ThisE <: E, A](
                                spec: TestSpec[A],
                                oi: Option[(String, DBInstance[ThisE])],
                                refValues: Map[TestIndex, Int],
                                R: HasRecovery[ThisE]
                              ) =
    spec.canRun(oi).mapSwitchF[ThisE, Unit]{
      case (name, instance) =>
        logger.info(s"[Starting test for $name] ")
        (for {
          _ <- doSetup[ThisE](spec.setup(_)(R, ec), instance, name)
          _ = logger.info(s"[Done setup for $name] ")
          _ <- runTestBatch(spec, instance, name, refValues, R)
          _ = logger.info(s"[Run test for $name] ")
        } yield ()).recover(_ => ())
    }(ec, R)

  private def runBatches[A](
                             spec: TestSpec[A],
                             reference: DBInstance[ER],
                             oi1: Option[(String, DBInstance[E1])],
                             oi2: Option[(String, DBInstance[E2])],
                             oi3: Option[(String, DBInstance[E3])],
                             oi4: Option[(String, DBInstance[E4])],
                             oi5: Option[(String, DBInstance[E5])],
                             oi6: Option[(String, DBInstance[E6])]
                           ): ConstrainedFuture[E, Unit] = {
    logger.info("Starting Setup")
    for {
      _ <- doSetup(spec.setup[ER], reference, "reference").eraseError
      _ = logger.info("[Starting reference batch]")
      refValues <- runReferenceBatch(spec, reference).eraseError
      _ = logger.info("[Done reference batch]")
      _ <- runOptionTest(spec, oi1, refValues, R1).eraseError
      _ <- runOptionTest(spec, oi2, refValues, R2).eraseError
      _ <- runOptionTest(spec, oi3, refValues, R3).eraseError
      _ <- runOptionTest(spec, oi4, refValues, R4).eraseError
      _ <- runOptionTest(spec, oi5, refValues, R5).eraseError
      _ <- runOptionTest(spec, oi6, refValues, R6).eraseError
    } yield ()
  }


  private def runBatch[ThisE <: E, A](
                   spec: TestSpec[A],
                   impl: DBInstance[ThisE],
                   instanceName: String,
                   R: HasRecovery[ThisE]
                 ): ConstrainedFuture[ThisE, Map[TestInstance, TimeResult[Int]]] =
    sequence(TestInstance(spec, instanceName)) {
      i =>
        logger.info(s"Running $i" )
        timeConstrainedFuture(spec.test(impl)(_)(R, ec))(i)
    }(R)

  private def runReferenceBatch[A](
                            spec: TestSpec[A],
                            impl: DBInstance[ER]
                          ): ConstrainedFuture[ER, Map[TestIndex, Int]] =
    for {
      instanceToTimedResult <- runBatch(spec, impl, "ReferenceInstance", RR)
      timeResults = instanceToTimedResult.map {case (t, res) => t.testIndex -> res}
      referenceResults = instanceToTimedResult.map {case (t, res) => t.testIndex -> res.a}
      _ = timeResults.values.foreach(logTimeResult(_, success = true))
      _ = logBatchResult(BatchedTimedResults(instanceToTimedResult.values.toSeq))
    } yield referenceResults.mapValues(_.hashCode())


  def logTimeResult[A](ta: TimeResult[A], success: Boolean): Unit = {
    if (success){
      logger.info(s"[Result][Success][${ta.instance.testBackend}]:${ta.instance}:time=${ns(ta.ns)}")
    } else {
      logger.info(s"[Result][Failure][${ta.instance.testBackend}]:${ta.instance}:time=${ns(ta.ns)}")
    }
  }

  private def logBatchResult[A](tas: BatchedTimedResults[A]): Unit = {
    logger.info(s"[Result][Batch]:test=${tas.testName.name}:impl=${tas.backend}:testcount=${tas.length}:totalTime=${ns(tas.fullTime)}")
  }

  private def runTestBatch[ThisE <: E, A](
                       spec: TestSpec[A],
                       impl: DBInstance[ThisE],
                       toTest: String,
                       expectedResults: Map[TestIndex, Int],
                       R: HasRecovery[ThisE]
                     ): ConstrainedFuture[ThisE, Unit] = {
    for {
      instanceToTimedResult <- runBatch(spec, impl, toTest, R)
      timeResults = instanceToTimedResult.map {case (t, res) => t.testIndex -> res}
      testResults = instanceToTimedResult.map {case (t, res) => t.testIndex -> res.a}
      isSuccess = testResults == expectedResults
      _ = timeResults.values.foreach(logTimeResult(_, success = isSuccess))
      _ = logBatchResult(BatchedTimedResults(instanceToTimedResult.values.toSeq))
    } yield ()
  }

  private def logError(e: E): Unit = {
    logger.error("Hit an error when setting up instances. Halting tests")
    logger.error(e.toString)
  }

  private def sequence[ThisE <: E, A, B](in: TraversableOnce[A])(f: A => ConstrainedFuture[ThisE, B])(implicit R: HasRecovery[ThisE]): ConstrainedFuture[ThisE, Map[A, B]]
  = {
    in.foldLeft(ConstrainedFuture.point[ThisE, Map[A, B]](Map.empty[A, B])){
      case (or, a) =>
        for {
          r <- or
          m <- f(a).map(b => r + (a -> b)).recover{e => logError(e); r}
        } yield m
    }
  }


  private def timeConstrainedFuture[ThisE, R](block: TestIndex => ConstrainedFuture[ThisE, R])(testInstance: TestInstance): ConstrainedFuture[ThisE, TimeResult[Int]] = {
    val t0 = System.nanoTime()
    for {
      r <- block(testInstance.testIndex)
      t1 = System.nanoTime()
    } yield TimeResult(testInstance, t1 - t0,  r.hashCode())
  }
}










