package impl.lmdb.common.interfaces

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}

import core.user.dsl.{DBDir, DatabaseAddress, E, Empty}
import core.user.interfaces._
import core.user.schema.SchemaDescription
import impl.lmdb.common
import org.lmdbjava.Env
import org.lmdbjava.Env._
import org.slf4j.Logger

import scala.concurrent.ExecutionContext
import scalaz.Scalaz._
import scalaz._


/**
  * Created by Al on 12/12/2017.
  * Bespoke LMDB backend for the system
  */

trait Backend extends DBBackend {
  def mkTempInstance(env: Env[ByteBuffer], sd: SchemaDescription, dir: Path)(implicit ec: ExecutionContext): LMDBInstance
  def mkNewInstance(env: Env[ByteBuffer], sd: SchemaDescription)(implicit ec: ExecutionContext): LMDBInstance
  def mkExistingInstance(env: Env[ByteBuffer], sd: SchemaDescription)(implicit ec: ExecutionContext): LMDBInstance

  val mapSize: Long = 1024 * 1024 * 1024

  override def open(address: DatabaseAddress, schema: SchemaDescription)(implicit e: ExecutionContext): \/[E, DBInstance] =
    try {
      val instance = getInstance(address, schema)
      Thread.sleep(2) // small sleep to allow LMDB to update its mapsize
      println("Env map size = " + instance.env.info().mapSize)

      for {
        _ <- instance.initialise()
      } yield instance
    } catch {case e: Throwable => common.errors.recoverLMDBException(e).left}

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

  private def getInstance(address: DatabaseAddress, schema: SchemaDescription)(implicit ec: ExecutionContext): LMDBInstance = {
    val env = create()
      // max db size of 32 GB on windows (windows file size = map size
      .setMapSize(mapSize)
      .setMaxDbs(numberOfTables(schema))
      .setMaxReaders(1024)

    address match {
      case Empty =>
        val dir = Files.createTempDirectory("GraphDB")
        dir.toFile.deleteOnExit()
        mkTempInstance(env.open(dir.toFile), schema, dir)

      case DBDir(dir, _, _) =>
        val directory = dir.toFile
        if (!directory.exists()) {
          directory.mkdirs()
          mkNewInstance(env.open(directory), schema)
        } else {
          mkExistingInstance(env.open(directory), schema)
        }
    }
  }
}


