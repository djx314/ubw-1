package assist.models

import org.h2.jdbcx.JdbcDataSource
import slick.driver.H2Driver.api._
import net.scalax.fsn.s2s._
import scala.concurrent.Await

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
  def id = column[Option[Long]]("id", O.PrimaryKey, O.AutoInc)
  def parent = column[Long]("mi_parent_id")
  def name = column[String]("mi_menu_name")
  def url = column[Option[String]]("url")

  def * = (id, parent, name, url) <> (MenuItem.tupled, MenuItem.unapply _)
}

object menuItemTq extends TableQuery(cons => new MenuItemTable(cons))

class MenuItemPermissionTable(tag: Tag) extends Table[MenuItemPermission](tag, "a_mi_auth_link") {
  def id = column[Option[Long]]("id", O.PrimaryKey, O.AutoInc)
  def menuItem = column[Option[Long]]("mi_id")
  def permission = column[Option[Long]]("au_id")

  def * = (id, menuItem, permission) <> (MenuItemPermission.tupled, MenuItemPermission.unapply _)

}

object menuItemPermissionTq extends TableQuery(cons => new MenuItemPermissionTable(cons))

object FsnRun extends App {

  val data = List(
    MenuItem(parent = 23L, name = "dfdsfhtrjh", url = Option("fdhtyjyukuili")),
    MenuItem(parent = 231L, name = "喵喵喵喵", url = Option("汪汪汪汪")),
    MenuItem(parent = 23L, name = "dfdsfh23423trjh", url = Option("fdhtyjyuk23423uili")),
    MenuItem(parent = 23L, name = "dfdsfh2342trjh", url = Option("34234234234"))
  )

  val sdb = {
    val datasource = new JdbcDataSource()
    datasource.setUrl(s"jdbc:h2:mem:source;DB_CLOSE_DELAY=-1")
    Database.forDataSource(datasource)
  }

  val tdb = {
    val datasource = new JdbcDataSource()
    datasource.setUrl(s"jdbc:h2:mem:target;DB_CLOSE_DELAY=-1")
    Database.forDataSource(datasource)
  }

  Await.result(sdb.run(menuItemTq.schema.create), scala.concurrent.duration.Duration.Inf)
  Await.result(tdb.run(menuItemTq.schema.create), scala.concurrent.duration.Duration.Inf)

  Await.result(sdb.run(menuItemTq ++= data), scala.concurrent.duration.Duration.Inf)

  val aa = new FConvert {}
  import aa._
  import scala.concurrent.ExecutionContext.Implicits.global

  val resultQuery = for {
    sTq <- new SourceQueryExtensionMethods(menuItemTq)
  } yield for {
    tTq <- new TargetQueryExtensionMethods(menuItemTq)
  } yield {
    List(sTq.parent setToSame tTq.parent, sTq.name setToSame tTq.name, sTq.url setToSame tTq.url)
    //List(sTq setToSame tTq)
  }

  Await.result(resultQuery.db2db(sdb, tdb), scala.concurrent.duration.Duration.Inf)

  println(Await.result(sdb.run(menuItemTq.result), scala.concurrent.duration.Duration.Inf).mkString("\n"))
  println(Await.result(tdb.run(menuItemTq.result), scala.concurrent.duration.Duration.Inf).mkString("\n"))

}