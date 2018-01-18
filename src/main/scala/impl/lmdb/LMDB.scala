package impl.lmdb

import java.nio.ByteBuffer
import java.nio.file.Files

import core.user.dsl.{DBDir, DatabaseAddress, E, Empty}
import core.user.interfaces._
import org.lmdbjava.Env._
import core.user.schema.SchemaDescription
import org.lmdbjava.Env

import scala.concurrent.ExecutionContext
import scalaz.Scalaz._
import scalaz._


/**
  * Created by Al on 12/12/2017.
  * Bespoke LMDB backend for the system
  */

object LMDB extends DBBackend {
  override def open(address: DatabaseAddress, schema: SchemaDescription)(implicit e: ExecutionContext): \/[E, DBInstance] =
    try {
      val (env, isNew) = initEnvironment(address, schema)
      Thread.sleep(2) // small sleep to allow LMDB to update its mapsize

      val instance = new LMDBInstance(env, schema, isNew)
      for {
        _ <- instance.initialise()
      } yield instance
    } catch {case e: Throwable => errors.recoverLMDBException(e).left}

  /**
    * Counts number of tables needed
    * @param schema - schema to analyse
    * @return
    */
  private def numberOfTables(schema: SchemaDescription): Int = {
    val base = 8 // required no matter what: default, availableViews, Commits, object, view counter, relations, reverse relations
    val numberOfUserTables = schema.objects.size
    val numberOfIndices = schema.objects.foldLeft(0) {
      _ + _.any.pattern.length + 1 // index for each column + empty index
    }

    base + numberOfUserTables + numberOfIndices
  }

  private def initEnvironment(address: DatabaseAddress, schema: SchemaDescription): (Env[ByteBuffer], Boolean) = {
    val env = create()
      .setMapSize(1024l * 1024l * 1024l * 1024l)
      .setMaxDbs(numberOfTables(schema))
      .setMaxReaders(1024)




    address match {
      case Empty =>
        val dir = Files.createTempDirectory("GraphDB")
        dir.toFile.deleteOnExit()
        (env.open(dir.toFile), true)

      case DBDir(dir, _, _) =>
        val directory = dir.toFile
        val isNew = if (!directory.exists()) {
          directory.mkdirs()
          true
        } else false
        (env.open(directory), isNew)
    }
  }
}


