package impl.sql

import core.backend.interfaces.{DBExecutor, DBInstance}
import core.error.E
import core.schema.TableName
import core.view.View
import impl.sql.tables.ObjectTable
import impl.sql.view.ViewsTable

import scalaz.\/
import scalikejdbc._

/**
  * An instance should hold connection pool settings
  * Represents a connection to a database
  */

class SQLInstance(connectionPool: ConnectionPool) extends DBInstance {
  // initialize JDBC driver & connection pool
  Class.forName("org.h2.Driver")
  ConnectionPool.singleton("jdbc:h2:mem:hello", "user", "pass")

  // ad-hoc session provider on the REPL
  implicit val session = AutoSession

  // table creation, you can run DDL by using #execute as same as JDBC
  sql"""
create table members (
  id serial not null primary key,
  name varchar(64),
  created_at timestamp not null
)
""".execute.apply()

  // insert initial data
  Seq("Alice", "Bob", "Chris") foreach { name =>
    sql"insert into members (name, created_at) values (${name}, current_timestamp)".update.apply()
  }

  // for now, retrieves all data as Map value
  val entities: List[Map[String, Any]] = sql"select * from members".map(_.toMap).list.apply()

  // defines entity object and extractor
  import org.joda.time._
  case class Member(id: Long, name: Option[String], createdAt: DateTime)
  object Member extends SQLSyntaxSupport[Member] {
    override val tableName = "members"
    def apply(rs: WrappedResultSet) = new Member(
      rs.long("id"), rs.stringOpt("name"), rs.jodaDateTime("created_at"))
  }

  // find all members
  val members: List[Member] = sql"select * from members".map(rs => Member(rs)).list.apply()

  // use paste mode (:paste) on the Scala REPL
  val m = Member.syntax("m")
  val name = "Alice"
  val alice: Option[Member] = withSQL {
    select.from(Member as m).where.eq(m.name, name)
  }.map(rs => Member(rs)).single.apply()




  /////////////////


  override val executor: DBExecutor = new SQLExecutor(this)

  override def setDefaultView(view: View): \/[E, Unit] = ???

  override def getDefaultView: \/[E, View] = ???

  // read from the views table
  override def getViews: Set[View] = DB readOnly {
    implicit session => sql"select view_id from VIEWS".map(rs => rs.long("view_id")).collection.apply()
  }



  private val viewsTable: ViewsTable = ???
  private val tables: Map[TableName, ObjectTable] = ???

}