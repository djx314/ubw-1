package net.scalax.fsn.database.test

import io.circe.syntax._

import net.scalax.fsn.core.FAtomicPathImpl
import net.scalax.fsn.json.operation.{ FDefaultAtomicHelper, FPropertyAtomicHelper }
import net.scalax.fsn.mix.helpers.{ Slick2JsonFsnImplicit, SlickCRUDImplicits }
import net.scalax.fsn.slick.helpers.{ FJsonAtomicHelper, FStrSelectExtAtomicHelper, StrFSSelectAtomicHelper }
import net.scalax.fsn.slick.model.{ ColumnOrder, JsonOut, JsonView, SlickParam }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import slick.jdbc.H2Profile.api._

import scala.concurrent._

object Sample09 extends SlickCRUDImplicits with StrFSSelectAtomicHelper with Slick2JsonFsnImplicit with App {

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
      "id" ofPile friend.id.out.order.describe("自增主键").readSlickComp.writeJ,
      "name" ofPile friend.name.out.filter.orderTarget("nick").describe("昵称").readSlickComp.writeJ,
      "nick" ofPile friend.nick.out.order.filter.describe("昵称").readSlickComp.writeJ,
      "ageOpt" ofPile friend.age.out.filter.readSlickComp.writeJ
    )
  }

  val result1: JsonOut = fQuery.filterResult
  val view1: DBIO[JsonView] = result1.toView(SlickParam())

  Await.result(Helper.db.run {
    Helper.initData
      .flatMap { _ =>
        view1.map { s =>
          Helper.prettyPrint(s)
        }
      }
  }, duration.Duration.Inf)

  val view2: DBIO[JsonView] = result1.toView(SlickParam(filter = Map("name" -> Map("eq" -> "魔理沙").asJson), orders = List(ColumnOrder("name", true), ColumnOrder("id", false), ColumnOrder("ageOpt", false))))

  Await.result(Helper.db.run {
    view2.map { s =>
      Helper.prettyPrint(s)
    }
  }, duration.Duration.Inf)

}