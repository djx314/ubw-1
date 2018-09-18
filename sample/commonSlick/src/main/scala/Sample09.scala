package net.scalax.ubw.database.test

import io.circe.syntax._
import net.scalax.ubw.core.{ AtomicPathImpl, AtomicValueImpl, PilesPolyHelper }
import net.scalax.ubw.json.operation.{ AtomicValueHelper, FDefaultAtomicHelper, FPropertyAtomicHelper, FSomeValue }
import net.scalax.ubw.mix.helpers.{ Slick2JsonFsnImplicit, SlickCRUDImplicits }
import net.scalax.ubw.slick.helpers.{ FJsonAtomicHelper, FStrSelectExtAtomicHelper, StrFSSelectAtomicHelper }
import net.scalax.ubw.slick.model.{ ColumnOrder, JsonView }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import slick.jdbc.H2Profile.api._

import scala.concurrent._

object Sample09 extends SlickCRUDImplicits with StrFSSelectAtomicHelper with Slick2JsonFsnImplicit with AtomicValueHelper with PilesPolyHelper {

  implicit def fPilesOptionImplicit[D](path: AtomicPathImpl[D]): FJsonAtomicHelper[D] with FStrSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] = {
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
      "id" ofPile friend.id.out.order.describe("自增主键").readJ.writeJ,
      "name" ofPile friend.name.out.filter.likeable.orderTarget("nick").describe("昵称").readJ.writeJ,
      "nick" ofPile friend.nick.out.order.filter.likeable.describe("昵称").readJ.writeJ,
      //"ageOpt" ofPile friend.age.out.filter.readSlickComp.writeJ,
      (("ageOpt" ofPile friend.age.out.filter.readJ))
        .poly("ageOpt1111" ofPile AtomicPathImpl.empty[Int].writeJ)
        .transform {
          case FSomeValue(t) => set(t.map(r => r + 2).getOrElse(1122))
          case AtomicValueImpl.Zero() => emptyValue[Int]
        }
    )
  }

  //val result1: JsonOut = fQuery.filterResult
  val view1: DBIO[JsonView] = fQuery.filterResult(Map.empty).toView

  Await.result(Helper.db.run {
    Helper.initData
      .flatMap { _ =>
        view1.map { s =>
          Helper.prettyPrint(s)
        }
      }
  }, duration.Duration.Inf)

  val view2: DBIO[JsonView] = fQuery.addOrders(List(ColumnOrder("name", true), ColumnOrder("id", false), ColumnOrder("ageOpt", false))).filterResult(Map("name" -> Map("like" -> "%魔%").asJson)).toView

  Await.result(Helper.db.run {
    view2.map { s =>
      Helper.prettyPrint(s)
    }
  }, duration.Duration.Inf)

}
