package net.scalax.fsn.core

import scala.reflect.runtime.universe._

trait FAtomic[+D]

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
    column.cols.find(par.isDefinedAt).map(par.apply)
  }

  def find[T](column: FColumn)(par: PartialFunction[FAtomic[column.DataType], T])(implicit typeTag: WeakTypeTag[T]): T = {
    findOpt(column)(par).getOrElse(throw new Exception(s"找不到匹配类型 ${typeTag.tpe} 的转换器"))
  }

  def filter[T](column: FColumn)(par: PartialFunction[FAtomic[column.DataType], T]): List[T] = {
    column.cols.filter(par.isDefinedAt).map(par.apply)
  }

}