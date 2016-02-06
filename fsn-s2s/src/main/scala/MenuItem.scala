package assist.models

import slick.driver.H2Driver.api._

case class MenuItem(
  id: Option[Long],
  parent: Long,
  name: String,
  url: Option[String],
  childrenUrl: Option[String],
  fileType: String,
  canHaveChild: Boolean,
  order: Double
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
  def childrenUrl = column[Option[String]]("mi_children_url")
  def fileType = column[String]("mi_file_type")
  def canHaveChild = column[Boolean]("mi_can_have_child")
  def order = column[Double]("mi_item_order")

  def * = (id, parent, name, url, childrenUrl, fileType, canHaveChild, order) <> (MenuItem.tupled, MenuItem.unapply _)
}

object menuItemTq extends TableQuery(cons => new MenuItemTable(cons))

class MenuItemPermissionTable(tag: Tag) extends Table[MenuItemPermission](tag, "a_mi_auth_link") {
  def id = column[Option[Long]]("id", O.PrimaryKey, O.AutoInc)
  def menuItem = column[Option[Long]]("mi_id")
  def permission = column[Option[Long]]("au_id")

  def * = (id, menuItem, permission) <> (MenuItemPermission.tupled, MenuItemPermission.unapply _)

}

object menuItemPermissionTq extends TableQuery(cons => new MenuItemPermissionTable(cons))