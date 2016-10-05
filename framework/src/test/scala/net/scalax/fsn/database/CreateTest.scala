package net.scalax.fsn.database.test

import net.scalax.fsn.mix.helpers.SlickCRUDImplicits
import net.scalax.fsn.slick.helpers.{FRep, FilterRepImplicitHelper}
import net.scalax.fsn.slick.model.{RWProperty, SlickParam}
import org.h2.jdbcx.JdbcDataSource
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class CreateTest extends FlatSpec
  with Matchers
  with EitherValues
  with ScalaFutures
  with BeforeAndAfterAll
  with BeforeAndAfter {

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

  after {
    db.run(friendTq.delete).futureValue
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

    //List[RWProperty]是用来 encode 成 json 格式的数据传递给浏览器的,让浏览器自动生成 crud 相关的逻辑
    friendQuery.properties shouldBe List(
      RWProperty(property = "id", typeName = "Long", inRetrieve = true, isAutoInc = true, isPrimaryKey = true),
      RWProperty("name", "java.lang.String", true, false, false),
      RWProperty("nick", "java.lang.String", true, false, false)
    )

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

  "slick create module" should "insert with non autoInc data" in {
    val friendQuery = for {
      inFriend <- friendTq.in
    } yield for {
      outFriend <- friendTq.out if outFriend.name === "xiaoxingxin"
    } yield {
      List(
        "extId" columns (((2, (), (())), ()), FRep(((2, (), (())), ()), inFriend).autoInc),
        "name" columns (outFriend.name.order, inFriend.fname),
        "nick" columns (outFriend.nick.order, inFriend.fnick)
      )
    }

    val friendWrap = friendQuery.result("id", true)

    //List[RWProperty]是用来 encode 成 json 格式的数据传递给浏览器的,让浏览器自动生成 crud 相关的逻辑
    friendQuery.properties shouldBe List(
      RWProperty("extId", "((Int, Unit, Unit), Unit)", true, true, false),
      RWProperty("name", "java.lang.String", true, false, false),
      RWProperty("nick", "java.lang.String", true, false, false)
    )

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
    friendFromDB.id.isDefined shouldBe false
    friendFromDB == friendFromJson shouldBe true

  }

}