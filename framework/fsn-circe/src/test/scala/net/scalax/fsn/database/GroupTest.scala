package net.scalax.fsn.database.test

import net.scalax.fsn.core.PilesPolyHelper
import net.scalax.fsn.mix.helpers.{ Slick2JsonFsnImplicit, SlickCRUDImplicits }
import net.scalax.fsn.slick.helpers.{ FRep, FilterRepImplicitHelper }
import net.scalax.fsn.slick.model._
import org.h2.jdbcx.JdbcDataSource
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class GroupTest extends FlatSpec
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

  val friendTq = TableQuery[FriendTable]

  lazy val db = {
    val datasource = new JdbcDataSource()
    datasource.setUrl(s"jdbc:h2:mem:groupTest;DB_CLOSE_DELAY=-1")
    Database.forDataSource(datasource, None)
  }

  override def beforeAll = {
    db.run(friendTq.schema.create).futureValue
  }

  override def afterAll = {
    db.run(friendTq.schema.drop).futureValue
  }

  before {
    db.run(friendTq ++= {
      List(
        Friend(None, "miaomiaomiaomiao", "abcjfiohgohgerg"),
        Friend(None, "miaomiaomiaomiao", "abcjfiohgohgerg"),
        Friend(None, "miaomiaomiaomiao", "afafewrgt"),
        Friend(None, "hahahahaha", "afafewrgt"),
        Friend(None, "hahahahaha", "afafewrgt"),
        Friend(None, "啊啊啊啊啊", "afafewrgt")
      )
    }).futureValue
  }

  after {
    db.run(friendTq.delete).futureValue
  }

  object helper extends Slick2JsonFsnImplicit
      with net.scalax.fsn.mix.slickbase.SqlRepImplicits
      with PilesPolyHelper
      with net.scalax.fsn.slick.helpers.StrFSSelectAtomicHelper
      with net.scalax.fsn.slick.helpers.GroupFSSelectAtomicHelper
      with SlickCRUDImplicits {

    import net.scalax.fsn.core.{ FAtomic }
    import net.scalax.fsn.json.operation.{ FDefaultAtomicHelper, FPropertyAtomicHelper }
    import net.scalax.fsn.slick.helpers.FStrSelectExtAtomicHelper
    import net.scalax.fsn.slick.helpers.{ FJsonAtomicHelper }

    implicit def fPilesOptionImplicit[D](columns: List[FAtomic[D]]) = {
      new FJsonAtomicHelper[D] with FStrSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] {
        override val atomics = columns
      }
    }

    implicit def atomicExtensionMethod2[D](atomic: FAtomic[D]) = {
      new FJsonAtomicHelper[D] with FStrSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] {
        override val atomics = atomic :: Nil
      }
    }

    implicit def atomicExtensionMethod3[D](atomic: FAtomic[D]): List[FAtomic[D]] = {
      atomic :: Nil
    }
  }

  import helper._

  "model" should "insert with json data" in {
    val plan11 = for {
      friend <- friendTq.out2222
    } yield {
      List(
        "abc" ofPile friend.id.groupOutput.groupWithNonOpt.writeJ.describe("喵喵"),
        "name" ofPile friend.name.groupOutput.nullsLast.writeJ.describe("喵喵"),
        "nick" ofPile friend.nick.groupOutput.nullsFirst.writeJ.describe("喵喵"),
        "intTest" ofPile 3.groupOutput.nullsLast.writeJ
      )
    }
    friendTq.map(_.id).sum

    val groupResult = plan11.groupResult(GroupParam(List("name", "nick", "intTest"), List(GroupColumn("abc", "avg"), GroupColumn("abc", "sum") /*, GroupColumn("paowa", "sum")*/ )))
    try {
      println(db.run(groupResult.resultAction).futureValue)
    } catch {
      case e: Exception =>
        e.printStackTrace
        throw e
    }

    /*val compare1 = friendTq.map(s => (s.id, s.name, s.nick)).sortBy(_._1.desc.nullsLast)
    val compare2 = friendTq.map(s => (s.id, s.name, s.nick)).sortBy(_._2.asc.nullsLast)
    val compare3 = friendTq.map(s => (s.id, s.name, s.nick)).sortBy(_._3.asc.nullsFirst)
    val compare4 = friendTq.map(s => (s.id, s.name, s.nick)).sortBy(_._2.desc.nullsLast).sortBy(_._1.asc.nullsLast).sortBy(_._3.asc.nullsFirst)

    (plan11.strResult("abc", true).statement(SlickParam())) shouldEqual (compare1.result.statements.toList)
    (plan11.strResult("name", false).statement(SlickParam())) shouldEqual (compare2.result.statements.toList)
    (plan11.strResult("nick", false).statement(SlickParam())) shouldEqual (compare3.result.statements.toList)
    (plan11.strResult("nick", false).statement(SlickParam(List(ColumnOrder("name", true), ColumnOrder("abc", false))))) shouldEqual (compare4.result.statements.toList)*/
  }

}

/*"model" should "insert with json data" in {
    val friendQuery = for {
      inFriend <- friendTq.crud
    } yield for {
      outFriend <- friendTq.out if outFriend.name === "xiaoxingxin"
    } yield {
      List(
        "id" columns (outFriend.id.out.order, inFriend.id.crud.primary.autoInc),
        "name" columns (outFriend.name.out.order, inFriend.name.crud),
        "nick" columns (outFriend.nick.out.order, inFriend.nick.crud)
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
      inFriend <- friendTq.crud
    } yield for {
      outFriend <- friendTq.out if outFriend.name === "xiaoxingxin"
    } yield {
      List(
        "extId" columns (((2, (), (())), ()).out, FRep(((2, (), (())), ()), inFriend).crud.autoInc),
        "name" columns (outFriend.name.out.order, inFriend.name.crud),
        "nick" columns (outFriend.nick.out.order, inFriend.nick.crud)
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

  }*/ 