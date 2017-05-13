package net.scalax.fsn.mix.helpers

import io.circe.{ Decoder, Encoder }
import net.scalax.fsn.common.atomic.FProperty
import net.scalax.fsn.core._
//import net.scalax.fsn.mix.helpers.{Select => SSelect}
import net.scalax.fsn.mix.slickbase.{ CrudQueryExtensionMethods, PileListQueryExtensionMethods }
import net.scalax.fsn.slick.helpers.{ FRep, SlickUtils }
import slick.lifted.{ FlatShapeLevel, Query, Shape }
import slick.relational.RelationalProfile

import scala.reflect.runtime.universe._
import scala.language.implicitConversions

trait SlickCRUDImplicits {

  trait FColumnStringImplicits {
    val proName1: String

    def ofPile[D](path: FAtomicPathImpl[D]): FPileImpl[FAtomicPathImpl[D], FAtomicValueImpl[D]] = {
      val proName = new FProperty[D] {
        override val proName = proName1
      }
      FPile.apply(FAtomicPathImpl(proName :: path.atomics))
    }
  }

  class FColumnStringImplicitsImpl(override val proName1: String) extends FColumnStringImplicits

  implicit def fColumnStringExtesionMethods(proName: String): FColumnStringImplicits = new FColumnStringImplicitsImpl(proName)

  implicit class slickColumn2CommonColumn[S, D, T](repLike: S)(
      implicit
      shape: Shape[_ <: FlatShapeLevel, S, D, T],
      encoder: Encoder[D],
      decoder: Decoder[D],
      weakTypeTag: WeakTypeTag[D]
  ) {
    def crud: SCRUD[S, D, T, D] = {
      SCRUD.in(repLike, SlickUtils.getTableIdFromCol(repLike)(shape))
    }
  }

  implicit class slickColumn2CRUDColumn[S, D, T](fRepLike: FRep[S])(
      implicit
      shape: Shape[_ <: FlatShapeLevel, S, D, T],
      encoder: Encoder[D],
      decoder: Decoder[D],
      weakTypeTag: WeakTypeTag[D]
  ) {
    def crud: SCRUD[S, D, T, D] = {
      SCRUD.in(fRepLike.rep, SlickUtils.getTableIdFromTable(fRepLike.owner))
    }
  }

  implicit def SCRUD2FAtomin[T](crud: SCRUD[_, _, _, T]): List[FAtomic[T]] = {
    crud.result
  }

  implicit class queryToUQueryExtendsionMethodGen[E, U](query: Query[E, U, Seq]) {

    def out = new PileListQueryExtensionMethods[E, U](query)

  }

  implicit class queryToCrudQueryExtendsionMethodGen[E <: RelationalProfile#Table[_], U](query: Query[E, U, Seq]) {

    def crud = new CrudQueryExtensionMethods[E, U](query)

  }

}