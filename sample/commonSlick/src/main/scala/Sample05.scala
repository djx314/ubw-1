package net.scalax.fsn.database.test

import io.circe.{ Json, Printer }
import io.circe.syntax._
import io.circe.generic.auto._
import net.scalax.fsn.core.FPathImpl
import net.scalax.fsn.json.operation.{ FDefaultAtomicHelper, FPropertyAtomicHelper }
import net.scalax.fsn.mix.helpers.{ Slick2JsonFsnImplicit, SlickCRUDImplicits }
import net.scalax.fsn.slick.helpers.{ FJsonAtomicHelper, FSelectExtAtomicHelper, StrFSSelectAtomicHelper }
import net.scalax.fsn.slick.model.{ JsonView, SlickParam }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import slick.jdbc.H2Profile.api._

import scala.concurrent._

object Sample05 extends SlickCRUDImplicits with StrFSSelectAtomicHelper with Slick2JsonFsnImplicit with App {

  val printer = Printer(
    preserveOrder = true,
    dropNullKeys = false,
    indent = " ",
    lbraceRight = " ",
    rbraceLeft = " ",
    lbracketRight = "\n",
    rbracketLeft = "\n",
    lrbracketsEmpty = "\n",
    arrayCommaRight = "\n",
    objectCommaRight = " ",
    colonLeft = " ",
    colonRight = " "
  )

  def prettyPrint(view: JsonView): Unit = {
    println("json data:\n" + view.data.asJson.pretty(printer) + "\n")
    println("properties:\n" + view.data.asJson.pretty(printer) + "\n")
  }

implicit def fPilesOptionImplicit[D](path: FPathImpl[D]): FJsonAtomicHelper[D] with FSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] = {
  val path1 = path
  new FJsonAtomicHelper[D] with FSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] {
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
    "ageOpt" ofPile friend.age.out.order.writeJ
  )
}

  Await.result(Sample01.db.run {
    Sample01.initData >>
      fQuery.strResult.toView(SlickParam()).map { s =>
        prettyPrint(s)
      }
  }, duration.Duration.Inf)

}