package net.scalax.ubw.database.test

import io.circe.Printer
import net.scalax.ubw.database.test.Sample01.{ friend1, friend2, friend3, friend4 }
import net.scalax.ubw.slick.model.JsonView
import org.h2.jdbcx.JdbcDataSource
import slick.jdbc.H2Profile.api._
import io.circe.syntax._
//import io.circe.generic.auto._

object Helper {

  lazy val db = {
    val datasource = new JdbcDataSource()
    datasource.setUrl(s"jdbc:h2:mem:commonSlick;DB_CLOSE_DELAY=-1")
    Database.forDataSource(datasource, None)
  }

  def initData: DBIO[Unit] = {
    FriendTable.schema.create >> (FriendTable ++= List(friend1, friend2, friend3, friend4)) >> DBIO.successful(())
  }

  val printer = Printer(
    preserveOrder = true,
    dropNullKeys = false,
    indent = " ",
    lbraceRight = " ",
    rbraceLeft = " ",
    lbracketRight = "\n",
    rbracketLeft = "\n",
    lrbracketsEmpty = "\n",
    arrayCommaRight = "\n",
    objectCommaRight = " ",
    colonLeft = " ",
    colonRight = " "
  )

  def prettyPrint(view: JsonView): Unit = {
    println("json data:\n" + view.data.asJson.pretty(printer) + "\n")
    //println("properties:\n" + view.properties.asJson.pretty(printer) + "\n")
  }

}