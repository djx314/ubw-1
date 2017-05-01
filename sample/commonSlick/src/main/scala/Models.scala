package net.scalax.fsn.database.test

import slick.jdbc.H2Profile.api._
import slick.lifted.Tag

case class Friend(
  id: Option[Long] = None,
  name: String,
  nick: String,
  age: Option[Int],
  grade: Int
)

class FriendTable(tag: Tag) extends Table[Friend](tag, "friend") {
  def id = column[Long]("id", O.AutoInc)
  def name = column[String]("name")
  def nick = column[String]("nick")
  def age = column[Option[Int]]("age")
  def grade = column[Int]("grade")

  def * = (id.?, name, nick, age, grade).mapTo[Friend]
}

object FriendTable extends TableQuery(cons => new FriendTable(cons))

class SimpleTable(tag: Tag, tbName: String) extends Table[Unit](tag, tbName) {
  def * = ()
}

object SimpleTable {

  def tq(tbName: String): TableQuery[SimpleTable] = {
    TableQuery(cons => new SimpleTable(cons, tbName))
  }

}