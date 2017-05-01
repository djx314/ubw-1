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

  class FColumnStringImplicits(proName1: String) {
    /*def column[D](converts: List[FAtomic[D]]): FsnColumn[D] = {
      val proName = new FProperty[D] {
        override val proName = proName1
      }
      FsnColumn(proName :: converts)
    }
    def column[D](converts: FAtomic[D]*): FsnColumn[D] = {
      val proName = new FProperty[D] {
        override val proName = proName1
      }
      FsnColumn(proName :: converts.toList)
    }
    def columns[D](converts: List[FAtomic[D]]*): FsnColumn[D] = {
      val proName = new FProperty[D] {
        override val proName = proName1
      }
      FsnColumn(proName :: converts.toList.flatten)
    }*/
    def ofPile[D](path: FPathImpl[D]): FPileImpl[FPathImpl[D], Option[D], Option] = {
      val proName = new FProperty[D] {
        override val proName = proName1
      }
      FPile.applyOpt(FPathImpl(proName :: path.atomics))(implicitly[FsnShape[FPathImpl[D], Option[D], FPathImpl[D], Option]])
    }
    /*def ofPath[D](path: FPathImpl[D]): FPathImpl[D] = {
      val proName = new FProperty[D] {
        override val proName = proName1
      }
      FPathImpl(proName :: path.atomics)
    }*/
  }

  implicit def fColumnStringExtesionMethods(proName: String): FColumnStringImplicits = new FColumnStringImplicits(proName)

  /*implicit class slickColumn2OutputColumn[S, D, T](repLike: S)(
    implicit
    shape1: Shape[_ <: FlatShapeLevel, S, D, T],
    encoder: Encoder[D],
    weakTypeTag: WeakTypeTag[D]
  ) {
    def out: SSelect[S, D, T, D] = {
      SSelect.out(repLike)
    }
  }*/

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

  /*implicit def slickFsnColumn2FAtomic[S, D, T](repLike: S)(
    implicit
    shape: Shape[_ <: FlatShapeLevel, S, D, T],
    encoder: Encoder[D],
    decoder: Decoder[D],
    weakTypeTag: WeakTypeTag[D]
  ): List[FAtomic[D]] = {
    SCRUD.in(repLike).result
  }*/

  implicit def SCRUD2FAtomin[T](crud: SCRUD[_, _, _, T]): List[FAtomic[T]] = {
    crud.result
  }

  /*implicit def slickFsnColumn2SelectColumn[S, D, T](repLike: S)(
    implicit
    shape1: Shape[_ <: FlatShapeLevel, S, D, T],
    encoder: Encoder[D],
    weakTypeTag: WeakTypeTag[D]
  ): SSelect[S, D, T, D] = {
    SSelect.out(repLike)
  }

  implicit def slickSelectColumn2FAtomic[S, D, T](repLike: S)(
    implicit
    shape1: Shape[_ <: FlatShapeLevel, S, D, T],
    encoder: Encoder[D],
    weakTypeTag: WeakTypeTag[D]
  ): List[FAtomic[D]] = {
    SSelect.out(repLike).result
  }*/

  /*implicit def Select2FAtomin[T](crud: SSelect[_, _, _, T]): List[FAtomic[T]] = {
    crud.result
  }*/

  implicit class queryToUQueryExtendsionMethodGen[E, U](query: Query[E, U, Seq]) {

    //def out = new ListQueryExtensionMethods[E, U](query)
    def out = new PileListQueryExtensionMethods[E, U](query)

  }

  implicit class queryToCrudQueryExtendsionMethodGen[E <: RelationalProfile#Table[_], U](query: Query[E, U, Seq]) {

    def crud = new CrudQueryExtensionMethods[E, U](query)

  }

}