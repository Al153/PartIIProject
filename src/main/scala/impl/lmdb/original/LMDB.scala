package impl.lmdb.original

import java.nio.ByteBuffer
import java.nio.file.Path

import core.user.schema.SchemaDescription
import impl.lmdb.common.interfaces.{Backend, LMDBInstance}
import org.lmdbjava.Env

import scala.concurrent.ExecutionContext


/**
  * Created by Al on 12/12/2017.
  * Bespoke LMDB backend for the system
  */

object LMDB extends Backend {
  override def mkTempInstance(env: Env[ByteBuffer], sd: SchemaDescription, dir: Path)(implicit ec: ExecutionContext): LMDBInstance =
    new TemporaryLMDBInstance(env, sd, dir)

  override def mkExistingInstance(env: Env[ByteBuffer], sd: SchemaDescription)(implicit ec: ExecutionContext): LMDBInstance =
    new ExistingOriginalLMDBInstance(env, sd)

  override def mkNewInstance(env: Env[ByteBuffer], sd: SchemaDescription)(implicit ec: ExecutionContext): LMDBInstance =
    new NewOriginalLMDBInstance(env, sd)

}


