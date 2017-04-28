package net.scalax.fsn.database.test

import org.h2.jdbcx.JdbcDataSource
import slick.jdbc.H2Profile.api._

import scala.concurrent.Future

object Sample01 {

  import scala.concurrent.ExecutionContext.Implicits.global

  lazy val db = {
    val datasource = new JdbcDataSource()
    datasource.setUrl(s"jdbc:h2:mem:commonSlick;DB_CLOSE_DELAY=-1")
    Database.forDataSource(datasource, None)
  }

  val friend1 = Friend(name = "魔理沙", nick = "小莎莎", age = Option(2333), grade = 3)
  val friend2 = Friend(name = "jilen", nick = "jilen 酱", age = Option(30), grade = 4)
  val friend3 = Friend(name = "品神", nick = "kerr", age = Option(28), grade = 5)
  val friend4 = Friend(name = "廖师虎", nick = "shihu", age = None, grade = 6)

  def initData: Future[Unit] = {
    db.run(FriendTable ++= List(friend1, friend2, friend3, friend4)).map(_ => ())
  }

}