package net.scalax.fsn.core

import net.scalax.fsn.mix.helpers.{SCRUD, Select => SSelect}
import io.circe.{Decoder, Encoder}
import net.scalax.fsn.common.FProperty
import net.scalax.fsn.slick.helpers.FRep
import slick.lifted.{FlatShapeLevel, Shape}

import scala.reflect.runtime.universe._
import scala.language.implicitConversions

trait FAtomic[D]

trait FColumn {

  type DataType
  val cols: List[FAtomic[DataType]]
  val data: Option[DataType]

}

case class FsnColumn[D](override val cols: List[FAtomic[D]], override val data: Option[D] = None) extends FColumn {
  override type DataType = D
}

object FColumn {

  type Aux[D] = FColumn { type DataType = D }

  def findOpt[T](column: FColumn)(par: PartialFunction[FAtomic[column.DataType], T]): Option[T] = {
    column.cols.find(par.isDefinedAt(_)).map(par.apply(_))
  }

  def find[T](column: FColumn)(par: PartialFunction[FAtomic[column.DataType], T])(implicit typeTag: WeakTypeTag[T]): T = {
    findOpt(column)(par).getOrElse(throw new Exception(s"找不到匹配类型 ${typeTag.tpe} 的转换器"))
  }

  def filter[T](column: FColumn)(par: PartialFunction[FAtomic[column.DataType], T]): List[T] = {
    column.cols.filter(par.isDefinedAt(_)).map(par.apply(_))
  }

}

trait FColumnImplicits {

  class FColumnStringImplicits(proName1: String) {
    def column[D](converts: List[FAtomic[D]]): FsnColumn[D] = {
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
    }
  }

  implicit def fColumnStringExtesionMethods(proName: String): FColumnStringImplicits = new FColumnStringImplicits(proName)

  implicit def slickFsnColumn2CRUDColumn[S, D, T](repLike: FRep[S])(
    implicit
    shape: Shape[_ <: FlatShapeLevel, S, D, T],
    encoder: Encoder[D],
    decoder: Decoder[D],
    weakTypeTag: WeakTypeTag[D]
  ): SCRUD[S, D, T, D] = {
    SCRUD.in(repLike)
  }

  implicit def slickFsnColumn2FAtomic[S, D, T](repLike: FRep[S])(
    implicit
    shape: Shape[_ <: FlatShapeLevel, S, D, T],
    encoder: Encoder[D],
    decoder: Decoder[D],
    weakTypeTag: WeakTypeTag[D]
  ): List[FAtomic[D]] = {
    SCRUD.in(repLike).result
  }

  implicit def SCRUD2FAtomin[T](crud: SCRUD[_, _, _, T]): List[FAtomic[T]] = {
    crud.result
  }

  implicit def slickFsnColumn2SelectColumn[S, D, T](repLike: S)(
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
  }

  implicit def Select2FAtomin[T](crud: SSelect[_, _, _, T]): List[FAtomic[T]] = {
    crud.result
  }

}