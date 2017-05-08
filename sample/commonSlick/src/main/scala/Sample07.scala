package net.scalax.fsn.database.test

import net.scalax.fsn.core.{ FAtomicPathImpl, PilesPolyHelper }
import net.scalax.fsn.json.operation.{ FDefaultAtomicHelper, FPropertyAtomicHelper }
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

object Sample07 extends SlickCRUDImplicits with StrFSSelectAtomicHelper with Slick2JsonFsnImplicit with PilesPolyHelper with App {

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
        (("nick" ofPile friend.nick.out.order.describe("昵称")) ::
          ("age" ofPile friend.age.out) :: FPNil) ::
          FPNil
      ).poly(
            "name" ofPile FAtomicPathImpl.empty[String].writeJ
          ).transform {
              case Some(name) :: (Some(nick) :: Some(Some(age)) :: HNil) :: HNil if age < 200 =>
                Option(s"$name-$nick")
              case Some(name) :: (_ :: _ :: HNil) :: HNil =>
                Option(name)
              case s =>
                //println(s)
                None
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
      //"id" ofPile friend.id.out.order.describe("自增主键").writeJ,
      {
        val aa = ((((
          ("name" ofPile friend.name.out.orderTarget("nick").describe("昵称")) ::
          ("nick" ofPile friend.nick.out.order.describe("昵称")) ::
          ("age" ofPile friend.age.out) ::
          FPNil
        ).poly(
            "name" ofPile FAtomicPathImpl.empty[String]
          ).transform {
              /*a =>
                println(a)
                val bb = a match {*/
              case Some(name) :: Some(nick) :: Some(Some(age)) :: HNil if age < 200 =>
                Option(s"$name-$nick")
              case Some(name) :: _ :: _ :: HNil =>
                Option(name)
              case _ =>
                None
              /*}
                bb*/
            }) :: ("ageOpt" ofPile friend.age.out) :: FPNil).poly("account" ofPile FAtomicPathImpl.empty[Aa]).transform {
              /*a =>
                println(a)
                a match {*/
              case Some(name) :: Some(Some(age)) :: HNil =>
                //println(Aa(name, age))
                Option(Aa(name, age))
              case _ =>
                None
              //}
            } :: ("id" ofPile friend.id.out.order.describe("自增主键")) :: ("id" ofPile friend.age.out.order.describe("年龄")) :: FPNil).poly("info" ofPile FAtomicPathImpl.empty[Map[String, Json]].writeJ).transform {
              case Some(aa) :: Some(id) :: Some(ageOpt) :: HNil =>
                Option(Map("id" -> id.asJson, "accountInfo" -> aa.asJson, "ageOpt" -> ageOpt.asJson))
              case _ :: Some(id) :: _ :: HNil =>
                Option(Map("message" -> s"id为${id}的不知名人事".asJson))
            }
        println(aa)
        aa
      } //,
    //"ageOpt" ofPile friend.age.out.writeJ
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
       { "id" : 1, "info" : { "id" : 1, "accountInfo" : { "name" : "魔理沙", "age" : 2333 } }, "ageOpt" : 2333 },
       { "id" : 2, "info" : { "id" : 2, "accountInfo" : { "name" : "jilen-jilen 酱", "age" : 30 } }, "ageOpt" : 30 },
       { "id" : 3, "info" : { "id" : 3, "accountInfo" : { "name" : "品神-kerr", "age" : 28 } }, "ageOpt" : 28 },
       { "id" : 4, "ageOpt" : null }
      ]
       */
    }
  }, duration.Duration.Inf)

}