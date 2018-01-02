package impl.lmdb

import java.io.File

import core.backend.interfaces.{DBBackend, DBInstance, DatabaseAddress}
import core.containers.ConstrainedFuture
import core.error.E
import core.schema.SchemaDescription
import org.fusesource.lmdbjni.Env

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 12/12/2017.
  * Bespoke LMDB backend for the system
  */

// todo: design key value system for system


object LMDB extends DBBackend {
  override def open(address: DatabaseAddress, schema: SchemaDescription)(implicit e: ExecutionContext): ConstrainedFuture[E, DBInstance] =
    ConstrainedFuture.point[E, DBInstance] {
      val env: Env = new Env("/tmp/mydb")
      new LMDBInstance(env, schema)
    }(errors.recoverLMDBException)

  def initEnvironment(): Env = {
    // val config = context.system.settings.config.getConfig("lmdb-journal")

    // val directory = new File(config.getString("dir"))
    // if (!directory.exists()) directory.mkdirs()

    val env = new Env()
    // env.setMaxDbs(config.getLong("maxdbs"))
    // env.open(config.getString("dir"))
    env
  }
}
