package net.scalax.fsn.database.test

import slick.jdbc.H2Profile.api._

case class Friend(
  id: Option[Long] = None,
  name: String,
  nick: String,
  age: Option[Int],
  grade: Int
)

class FriendTable(tag: Tag) extends Table[Friend](tag, "firend") {
  def id = column[Long]("id", O.AutoInc)
  def name = column[String]("name")
  def nick = column[String]("nick")
  def age = column[Option[Int]]("age")
  def grade = column[Int]("grade")

  def * = (id.?, name, nick, age, grade).mapTo[Friend]
}

object FriendTable extends TableQuery(cons => new FriendTable(cons))

class SimpleTable(tag: Tag) extends Table[Unit](tag, "student") {
  def * = ()
}