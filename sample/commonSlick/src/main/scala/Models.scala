package net.scalax.fsn.database.test

import slick.jdbc.H2Profile.api._

case class Friend(
  id: Option[Long],
  name: String,
  nick: String
)

class FriendTable(tag: Tag) extends Table[Friend](tag, "firend") {
  def id = column[Long]("id", O.AutoInc)
  def name = column[String]("name")
  def nick = column[String]("nick")

  def * = (id.?, name, nick).mapTo[Friend]
}