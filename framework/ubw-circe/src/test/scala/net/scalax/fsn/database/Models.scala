package net.scalax.ubw.database.test

import slick.jdbc.H2Profile.api._
/**
 * Created by djx314 on 15-6-22.
 */

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