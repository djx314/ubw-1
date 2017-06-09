package net.scalax.fsn.database.test

import io.circe.syntax._
import net.scalax.fsn.core.{ FAtomicPathImpl, FAtomicValueImpl, PilesPolyHelper }
import net.scalax.fsn.json.operation.{ FAtomicValueHelper, FDefaultAtomicHelper, FPropertyAtomicHelper, FSomeValue }
import net.scalax.fsn.mix.helpers.{ Slick2JsonFsnImplicit, SlickCRUDImplicits }
import net.scalax.fsn.slick.helpers.{ FJsonAtomicHelper, FStrSelectExtAtomicHelper, StrFSSelectAtomicHelper }
import net.scalax.fsn.slick.model.{ ColumnOrder, JsonOut, JsonView, SlickParam }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import slick.jdbc.H2Profile.api._

import scala.concurrent._

object Sample09 extends SlickCRUDImplicits with StrFSSelectAtomicHelper with Slick2JsonFsnImplicit with FAtomicValueHelper with PilesPolyHelper {

  implicit def fPilesOptionImplicit[D](path: FAtomicPathImpl[D]): FJsonAtomicHelper[D] with FStrSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] = {
    val path1 = path
    new FJsonAtomicHelper[D] with FStrSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] {
      override val path = path1
    }
  }

  FriendTable.filter(_.name like "aabb")

  val fQuery = for {
    friend <- FriendTable.out
  } yield {
    List(
      "id" ofPile friend.id.out.order.describe("自增主键").readSlickComp.writeJ,
      "name" ofPile friend.name.out.filter.likeable.orderTarget("nick").describe("昵称").readSlickComp.writeJ,
      "nick" ofPile friend.nick.out.order.filter.likeable.describe("昵称").readSlickComp.writeJ,
      //"ageOpt" ofPile friend.age.out.filter.readSlickComp.writeJ,
      (("ageOpt" ofPile friend.age.out.filter.readSlickComp))
        .poly("ageOpt1111" ofPile FAtomicPathImpl.empty[Int].writeJ)
        .transform {
          case FSomeValue(t) => set(t.map(r => r + 2).getOrElse(1122))
          case FAtomicValueImpl.Zero => emptyValue[Int]
        }
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

  val view2: DBIO[JsonView] = result1.toView(SlickParam(filter = Map("name" -> Map("like" -> "%魔%").asJson), orders = List(ColumnOrder("name", true), ColumnOrder("id", false), ColumnOrder("ageOpt", false))))

  Await.result(Helper.db.run {
    view2.map { s =>
      Helper.prettyPrint(s)
    }
  }, duration.Duration.Inf)

}
