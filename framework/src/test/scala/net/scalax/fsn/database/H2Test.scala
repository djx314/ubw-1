package net.scalax.fsn.database.test

import net.scalax.fsn.mix.helpers.SlickCRUDImplicits
import net.scalax.fsn.slick.helpers.FilterRepImplicitHelper
import net.scalax.fsn.slick.model.SlickParam
import org.h2.jdbcx.JdbcDataSource
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class H2Test extends FlatSpec
  with Matchers
  with EitherValues
  with ScalaFutures
  with BeforeAndAfterAll {

  val t = 10.seconds
  override implicit val patienceConfig = PatienceConfig(timeout = t)
  val logger = LoggerFactory.getLogger(getClass)

  object SlickCRUDImplicits extends SlickCRUDImplicits with FilterRepImplicitHelper

  import io.circe.generic.auto._, io.circe.syntax._, io.circe.parser._, io.circe._
  import slick.jdbc.H2Profile.api._
  import SlickCRUDImplicits._

  val friendTq = TableQuery[FriendTable]

  lazy val db = {
    val datasource = new JdbcDataSource()
    datasource.setUrl(s"jdbc:h2:mem:hfTest;DB_CLOSE_DELAY=-1")
    Database.forDataSource(datasource)
  }

  override def beforeAll = {
    db.run(friendTq.schema.create).futureValue
  }

  "model" should "insert with json data" in {
    val friendQuery = for {
      inFriend <- friendTq.in
    } yield for {
      outFriend <- friendTq.out if outFriend.name === "xiaoxingxin"
    } yield {
      List(
        "id" columns (outFriend.id.order, inFriend.fid.primary.autoInc),
        "name" columns (outFriend.name.order, inFriend.fname),
        "nick" columns (outFriend.nick.order, inFriend.fnick)
      )
    }

    val friendWrap = friendQuery.result("id", true)

    val jsonData = parse(
      """{ "name": "xiaoxingxin", "nick": "nvzhuang" }""").toOption.get
    try {
      val action = friendWrap.insertGen(jsonData.as[Map[String, Json]].toOption.get)
      val result = db.run(action).futureValue
      result.effectRows shouldBe 1
    } catch {
      case e: Exception => e.printStackTrace
    }

    val friendFromDB = db.run(friendWrap.jsonGen.data(SlickParam())
      .map(_._1.head)
      .map { s =>
        s.asJson.as[Friend].toOption.get
      }
    ).futureValue
    val friendFromJson = jsonData.as[Friend].toOption.get
    friendFromDB.id.isDefined shouldBe true
    friendFromDB == friendFromJson shouldBe false
    friendFromDB.copy(id = None) shouldBe friendFromJson

  }

}