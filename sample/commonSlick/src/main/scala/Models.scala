package net.scalax.fsn.database.test

import slick.jdbc.H2Profile.api._

case class Friend(
  id: Option[Long],
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

object FriendTq extends TableQuery(cons => new FriendTable(cons))

object Sample01 {

  lazy val db = {
    val datasource = new JdbcDataSource()
    datasource.setUrl(s"jdbc:h2:mem:groupTest;DB_CLOSE_DELAY=-1")
    Database.forDataSource(datasource, None)
  }

}