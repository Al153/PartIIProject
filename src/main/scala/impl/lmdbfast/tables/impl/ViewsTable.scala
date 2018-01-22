package impl.lmdbfast.tables.impl

import java.nio.ByteBuffer

import core.user.dsl.View
import impl.lmdbfast.access.Commit
import impl.lmdbfast.access.Key._
import impl.lmdbfast.access.Storable._
import impl.lmdbfast.tables.interfaces.LMDBTable
import impl.lmdbfast.{LMDBEither, LMDBInstance, _}
import org.lmdbjava.Dbi
import org.lmdbjava.DbiFlags._

/**
  * Created by Al on 28/12/2017.
  *
  * Relates views to commits
  */
// todo: move away from the FixedPointTraversal, since we only ever append to the collection of commits or traverse it.
//       so a list might be quicker
// todo: These values are immutable so can be easily cached
class ViewsTable(implicit val instance: LMDBInstance) extends LMDBTable {
  override def name: String = "db:views"
  override val db: Dbi[ByteBuffer] = instance.env.openDbi(name, MDB_CREATE)

  /**
    * Initialisation consists of setting the initial view to contain the empty set of commits
    */
  override def initialise(): LMDBEither[Unit] = newChildView(initialView, List())

  /**
    * Lookup commits associated with a view
    * @param v - view to lookup
    * @return
    */
  def lookupCommits(v: View): LMDBEither[List[Commit]] = get[List[Commit]](v.key)

  /**
    * Insert a new child view  with its commits
    */
  def newChildView(newView: View, commits: List[Commit]): LMDBEither[Unit] = put[List[Commit]](newView.key, commits)
}
