package net.scalax.fsn.database.test

import net.scalax.fsn.core.FAtomicPathImpl
import net.scalax.fsn.json.operation.{ FDefaultAtomicHelper, FPropertyAtomicHelper }
import net.scalax.fsn.mix.helpers.{ Slick2JsonFsnImplicit, SlickCRUDImplicits }
import net.scalax.fsn.slick.helpers.{ FJsonAtomicHelper, FStrSelectExtAtomicHelper, StrFSSelectAtomicHelper }
import net.scalax.fsn.slick.model.{ ColumnOrder, JsonOut, JsonView, SlickParam }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import slick.jdbc.H2Profile.api._

import scala.concurrent._

object Sample05 extends SlickCRUDImplicits with StrFSSelectAtomicHelper with Slick2JsonFsnImplicit {

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
      "name" ofPile friend.name.out.orderTarget("nick").describe("昵称").writeJ,
      "nick" ofPile friend.nick.out.order.describe("昵称").writeJ,
      "ageOpt" ofPile friend.age.out.writeJ
    )
  }

  val result1: JsonOut = fQuery.strResult
  val view1: DBIO[JsonView] = result1.toView(SlickParam())

  Await.result(Helper.db.run {
    Helper.initData
      .flatMap { _ =>
        view1.map { s =>
          Helper.prettyPrint(s)
        }
      }
  }, duration.Duration.Inf)

  val view2: DBIO[JsonView] = result1.toView(SlickParam(orders = List(ColumnOrder("name", true), ColumnOrder("id", false), ColumnOrder("ageOpt", false))))

  Await.result(Helper.db.run {
    view2.map { s =>
      Helper.prettyPrint(s)
    }
  }, duration.Duration.Inf)

}