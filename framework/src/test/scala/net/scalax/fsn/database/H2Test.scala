package net.scalax.fsn.database.test

import org.h2.jdbcx.JdbcDataSource
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.slf4j.LoggerFactory

import scala.concurrent.duration._

class H2Test extends FlatSpec
  with Matchers
  with EitherValues
  with ScalaFutures
  with BeforeAndAfterAll {

  val t = 10.seconds
  override implicit val patienceConfig = PatienceConfig(timeout = t)
  val logger = LoggerFactory.getLogger(getClass)

  import io.circe.generic.auto._, io.circe.syntax._, io.circe.parser._
  import net.scalax.fsn.mix.helpers.In
  import slick.jdbc.H2Profile.api._

  val friendTq = TableQuery[FriendTable]

  lazy val db = {
    val datasource = new JdbcDataSource()
    datasource.setUrl(s"jdbc:h2:mem:hfTest;DB_CLOSE_DELAY=-1")
    Database.forDataSource(datasource)
  }

  override def beforeAll = {
  }

  "test" should "effect some converage" in {
    In.jRead[String]
  }

}