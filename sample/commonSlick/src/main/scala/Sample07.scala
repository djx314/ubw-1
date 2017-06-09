package net.scalax.fsn.database.test

import net.scalax.fsn.core.{ FAtomicPathImpl, PilesPolyHelper }
import net.scalax.fsn.json.operation._
import net.scalax.fsn.mix.helpers.{ Slick2JsonFsnImplicit, SlickCRUDImplicits }
import net.scalax.fsn.slick.helpers.{ FJsonAtomicHelper, FStrSelectExtAtomicHelper, StrFSSelectAtomicHelper }
import net.scalax.fsn.slick.model._
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import slick.jdbc.H2Profile.api._
import shapeless._

import scala.concurrent._

object Sample07 extends SlickCRUDImplicits
    with StrFSSelectAtomicHelper
    with Slick2JsonFsnImplicit
    with PilesPolyHelper
    with FAtomicValueHelper
    with App {

  implicit def fPilesOptionImplicit[D](path: FAtomicPathImpl[D]): FJsonAtomicHelper[D] with FStrSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] = {
    val path1 = path
    new FJsonAtomicHelper[D] with FStrSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] {
      override val path = path1
    }
  }

  val fQuery = for {
    friend <- FriendTable.out
  } yield {
    List(
      "id" ofPile friend.id.out.order.describe("自增主键").writeJ,
      (
        ("name" ofPile friend.name.out.orderTarget("nick").describe("昵称")) ::
        ("nick" ofPile friend.nick.out.order.describe("昵称")) ::
        ("age" ofPile friend.age.out) ::
        FPNil
      ).poly(
          "name" ofPile FAtomicPathImpl.empty[String].writeJ
        ).transform {
            case FSomeValue(name) :: FSomeValue(nick) :: FSomeValue(Some(age)) :: HNil if age < 200 =>
              set(s"${name}-${nick}")
            case (name @ FSomeValue(_)) :: FSomeValue(_) :: FSomeValue(_) :: HNil =>
              name
            case _ =>
              emptyValue[String]
          },
      "ageOpt" ofPile friend.age.out.writeJ
    )
  }

  val result1: JsonOut = fQuery.strResult

  val view1: DBIO[JsonView] = result1.toView(SlickParam(orders = List(ColumnOrder("name", true), ColumnOrder("id", false), ColumnOrder("ageOpt", false))))

  Await.result(Helper.db.run {
    Helper.initData
      .flatMap { _ =>
        view1.map { s =>
          Helper.prettyPrint(s)
        }
      }
  }, duration.Duration.Inf)

  case class Aa(name: String, age: Int)

  val moreComplexQuery = for {
    friend <- FriendTable.out
  } yield {
    List(
      "id" ofPile friend.id.out.order.describe("自增主键").writeJ,
      ((((
        ("name" ofPile friend.name.out.orderTarget("nick").describe("昵称")) ::
        ("nick" ofPile friend.nick.out.order.describe("昵称")) ::
        ("age" ofPile friend.age.out) ::
        FPNil
      ).poly(
          "name" ofPile FAtomicPathImpl.empty[String]
        ).transform {
            case FSomeValue(name) :: FSomeValue(nick) :: FSomeValue(Some(age)) :: HNil if age < 200 =>
              set(s"${name}-${nick}")
            case FSomeValue(name) :: _ :: _ :: HNil =>
              set(name)
            case _ =>
              emptyValue[String]
          }) :: ("ageOpt" ofPile friend.age.out) :: FPNil).poly("account" ofPile FAtomicPathImpl.empty[Aa]).transform { t =>
            t match {
              case FSomeValue(name) :: FSomeValue(Some(age)) :: HNil =>
                set(Aa(name, age))
              case _ =>
                emptyValue[Aa]
                set(Aa("喵", 2334455))
            }
          } :: ("id" ofPile friend.id.out.order.describe("自增主键")) :: ("id" ofPile friend.age.out.order.describe("年龄")) :: FPNil).poly("info" ofPile FAtomicPathImpl.empty[Map[String, Json]].writeJ).transform { t =>
            t match {
              case FSomeValue(aa) :: FSomeValue(id) :: FSomeValue(ageOpt) :: HNil =>
                set(Map("id" -> id.asJson, "accountInfo" -> aa.asJson, "ageOpt" -> ageOpt.asJson))
              case _ :: FSomeValue(id) :: _ :: HNil =>
                set(Map("message" -> s"id为${id}的不知名人事".asJson))
            }
          },
      "ageOpt" ofPile friend.age.out.writeJ
    )
  }

  val result2: JsonOut = moreComplexQuery.strResult

  val view2: DBIO[JsonView] = result2.toView(SlickParam(orders = List(ColumnOrder("name", true), ColumnOrder("id", false), ColumnOrder("ageOpt", false))))

  Await.result(Helper.db.run {
    view2.map { s =>
      Helper.prettyPrint(s)
      /*
      json data:
        [
         { "id" : 3, "info" : { "id" : 3, "accountInfo" : { "name" : "品神-kerr", "age" : 28 }, "ageOpt" : 28 }, "ageOpt" : 28 },
         { "id" : 2, "info" : { "id" : 2, "accountInfo" : { "name" : "jilen-jilen 酱", "age" : 30 }, "ageOpt" : 30 }, "ageOpt" : 30 },
         { "id" : 1, "info" : { "id" : 1, "accountInfo" : { "name" : "魔理沙", "age" : 2333 }, "ageOpt" : 2333 }, "ageOpt" : 2333 },
         { "id" : 4, "info" : { "message" : "id为4的不知名人事" }, "ageOpt" : null }
        ]

       */
    }
  }, duration.Duration.Inf)

}