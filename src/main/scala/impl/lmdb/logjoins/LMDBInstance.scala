package impl.lmdb.logjoins

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription
import core.utils._
import impl.lmdb.common.LMDBEither
import impl.lmdb.common.interfaces.{LMDBExecutor, LMDBInstance}
import org.lmdbjava.Env

import scala.concurrent.ExecutionContext
import scalaz.Scalaz._

/**
  * Created by Al on 12/12/2017.
  *
  * [[DBInstance]] implementation
  */


/**
  * A temporary instance wants to delete its containing folder on closure, and do a full init
  */
final class TemporaryLogInstance(e: Env[ByteBuffer], schema: SchemaDescription, dir: Path)(implicit ec: ExecutionContext) extends LMDBInstance(e, schema) {
  override lazy val executor: LMDBExecutor = new LogExecutor()

  override def initialise(): LMDBEither[Unit] = for {
    _ <- controlTables.availableViews.initialise()
    _ <- controlTables.commitsCounter.initialise()
    _ <- controlTables.defaultView.initialise()
    _ <- controlTables.relations.initialise()
    _ <- controlTables.reverseRelations.initialise()
    _ <- controlTables.viewsCounter.initialise()
    _ <- controlTables.viewsTable.initialise()
    _ <- controlTables.objectCounter.initialise()
    _ <- EitherOps.sequence(objects.map {case (_, table) => table.initialise()})
  } yield ()


  /**
    * Close DB and env
    */
  override def close(): Unit = {
    env.close()

    // recursively delete the database upon closure

    Files.walkFileTree(dir, new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }
      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        Files.delete(dir)
        FileVisitResult.CONTINUE
      }
    })
  }
}

/**
  * A persistent LMDBInstance for a new database, needs initialisation, shouldn't delete on exit
  */
final class NewLogInstance(
                             e: Env[ByteBuffer],
                             s: SchemaDescription
                           )(implicit ec: ExecutionContext) extends LMDBInstance(e, s) {

  override lazy val executor: LMDBExecutor = new LogExecutor()
  override def initialise(): LMDBEither[Unit] = for {
    _ <- controlTables.availableViews.initialise()
    _ <- controlTables.commitsCounter.initialise()
    _ <- controlTables.defaultView.initialise()
    _ <- controlTables.relations.initialise()
    _ <- controlTables.reverseRelations.initialise()
    _ <- controlTables.viewsCounter.initialise()
    _ <- controlTables.viewsTable.initialise()
    _ <- controlTables.objectCounter.initialise()
    _ <- EitherOps.sequence(objects.map {case (_, table) => table.initialise()})
  } yield ()

  /**
    * Close the database
    */
  override def close(): Unit = env.close()
}

/**
  * LMDBInstance for existing database, no initialisation or closure needed
  */
final class ExistingLogInstance(
                                  e: Env[ByteBuffer],
                                  s: SchemaDescription
                                )(implicit ec: ExecutionContext) extends LMDBInstance(e, s) {
  println("Existing!")
  override lazy val executor: LMDBExecutor = new LogExecutor()
  override def initialise(): LMDBEither[Unit] = ().right
  /**
    * Close the database
    */
  override def close(): Unit = env.close()
}
