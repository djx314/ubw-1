package net.scalax.ubw.database.test

import net.scalax.ubw.core.AtomicPathImpl
import net.scalax.ubw.json.operation.{ FDefaultAtomicHelper, FPropertyAtomicHelper }
import net.scalax.ubw.mix.helpers.{ Slick2JsonFsnImplicit, SlickCRUDImplicits }
import net.scalax.ubw.slick.helpers.{ FJsonAtomicHelper, FStrSelectExtAtomicHelper, StrFSSelectAtomicHelper }
import net.scalax.ubw.slick.model._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import slick.jdbc.H2Profile.api._

import scala.concurrent._

object Sample06 extends SlickCRUDImplicits with StrFSSelectAtomicHelper with Slick2JsonFsnImplicit {

  implicit def fPilesOptionImplicit[D](path: AtomicPathImpl[D]): FJsonAtomicHelper[D] with FStrSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] = {
    val path1 = path
    new FJsonAtomicHelper[D] with FStrSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] {
      override val path = path1
    }
  }

  val fQuery = for {
    friend <- FriendTable.out
  } yield {
    List(
      "id" ofPile friend.id.out.order.describe("自增主键").inView(false).writeJ,
      "name" ofPile friend.name.out.orderTarget("nick").describe("昵称").writeJ,
      "nick" ofPile friend.nick.out.order.describe("昵称").inView(false).writeJ,
      "ageOpt" ofPile friend.age.out.writeJ
    )
  }

  val result1: JsonOut = fQuery.strResult

  val view1: DBIO[JsonView] = fQuery.addOrders(List(ColumnOrder("name", true), ColumnOrder("id", false), ColumnOrder("ageOpt", false))).strResult.toView

  Await.result(Helper.db.run {
    Helper.initData
      .flatMap { _ =>
        view1.map { s =>
          Helper.prettyPrint(s)
        }
      }
  }, duration.Duration.Inf)

}