package assist.models

import slick.driver.H2Driver.api._
import net.scalax.fsn.s2s._
import scala.concurrent.Await
import shapeless._

case class MenuItem(
  id: Option[Long] = None,
  parent: Long,
  name: String,
  url: Option[String]
)

case class MenuItemPermission(
  id: Option[Long],
  menuItem: Option[Long],
  permission: Option[Long]
)

class MenuItemTable(tag: Tag) extends Table[MenuItem](tag, "a_menu_item") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def parent = column[Long]("mi_parent_id")
  def name = column[String]("mi_menu_name")
  def url = column[Option[String]]("url")

  def * = (id.?, parent, name, url) <> (MenuItem.tupled, MenuItem.unapply _)
}

object menuItemTq extends TableQuery(cons => new MenuItemTable(cons))

class MenuItemPermissionTable(tag: Tag) extends Table[MenuItemPermission](tag, "a_mi_auth_link") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def menuItem = column[Option[Long]]("mi_id")
  def permission = column[Option[Long]]("au_id")

  def * = (id.?, menuItem, permission) <> (MenuItemPermission.tupled, MenuItemPermission.unapply _)

}

object menuItemPermissionTq extends TableQuery(cons => new MenuItemPermissionTable(cons))

object FsnRun extends App {

  val data = List(
    MenuItem(parent = 23L, name = "dfdsfhtrjh", url = Option("fdhtyjyukuili")),
    MenuItem(parent = 231L, name = "喵喵喵喵", url = Option("汪汪汪汪")),
    MenuItem(parent = 679518L, name = "dfdsfh23423trjh", url = Option("fdhtyjyuk23423uili")),
    MenuItem(parent = 8524961L, name = "dfdsfh2342trjh", url = None)
  )

  val sdb = {
    //val datasource = new JdbcDataSource()
    //datasource.setUrl(s"jdbc:jdbcdslog:h2:mem:source;DB_CLOSE_DELAY=-1;targetDriver=org.h2.Driver")
    //datasource.setURL("org.jdbcdslog.DriverLoggingProxy")
    Database.forURL(
      url = "jdbc:jdbcdslog:h2:mem:source;DB_CLOSE_DELAY=-1;targetDriver=org.h2.Driver",
      driver = "org.jdbcdslog.DriverLoggingProxy"
    )
  }

  val tdb = {
    /*val datasource = new JdbcDataSource()
    datasource.setUrl(s"jdbc:h2:mem:target;DB_CLOSE_DELAY=-1")
    Database.forDataSource(datasource)*/
    Database.forURL(
      url = "jdbc:jdbcdslog:h2:mem:target;DB_CLOSE_DELAY=-1;targetDriver=org.h2.Driver",
      driver = "org.jdbcdslog.DriverLoggingProxy"
    )
  }

  Await.result(sdb.run(menuItemTq.schema.create), scala.concurrent.duration.Duration.Inf)
  Await.result(tdb.run(menuItemTq.schema.create), scala.concurrent.duration.Duration.Inf)

  Await.result(sdb.run(menuItemTq ++= data), scala.concurrent.duration.Duration.Inf)

  import scala.concurrent.ExecutionContext.Implicits.global

  val resultQuery = for {
    sTq <- menuItemTq.in
    tTq <- menuItemTq.out
  } yield {
    List(
      sTq.parent setToSame tTq.parent,
      (sTq.parent :: sTq.name :: HNil).setTo(tTq.name).apply { case id :: name :: HNil => name + id.toString },
      (sTq.parent :: sTq.name :: sTq.url :: HNil).setTo(tTq.url).apply { case parent :: name :: url :: HNil => url.map(s => parent.toString + name + s) }
    )
  }

  Await.result(resultQuery.db2db(sdb, tdb), scala.concurrent.duration.Duration.Inf)

  println(Await.result(sdb.run(menuItemTq.result), scala.concurrent.duration.Duration.Inf).mkString("\n"))
  println(Await.result(tdb.run(menuItemTq.result), scala.concurrent.duration.Duration.Inf).mkString("\n"))

}