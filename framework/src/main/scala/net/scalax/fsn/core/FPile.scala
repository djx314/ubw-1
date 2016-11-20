package net.scalax.fsn.core

import scala.reflect.runtime.universe._
import scala.language.higherKinds
import scala.language.implicitConversions

trait FPath {
  type DataType
  val atomics: List[FAtomic[DataType]]
}

object FPath {
  def findOpt[T](path: FPath)(par: PartialFunction[FAtomic[path.DataType], T]): Option[T] = {
    path.atomics.find(par.isDefinedAt).map(par.apply)
  }

  def find[T](path: FPath)(par: PartialFunction[FAtomic[path.DataType], T])(typeTag: WeakTypeTag[T]): T = {
    findOpt(path)(par).getOrElse(throw new Exception(s"找不到匹配类型 ${typeTag.tpe} 的转换器"))
  }

  def filter[T](path: FPath)(par: PartialFunction[FAtomic[path.DataType], T]): List[T] = {
    path.atomics.filter(par.isDefinedAt).map(par.apply)
  }

  def translate[T](pathTran: FPath => T)(implicit aa: String) = {

  }
}

case class FPathImpl[D](override val atomics: List[FAtomic[D]]) extends FPath {
  override type DataType = D
}

trait FPathShape[Source, Target] {

  def apply(path: FPath, source: Source): Target

}

trait FPile[E, U, C] {
  self =>

  type PathType = E
  type DataType = U
  type WrapType = C

  val pathPile: PathType
  //val wrapPile: WrapType

  val fShape: FsnShape[PathType, DataType, WrapType]

  def copyWrapPile(dataList: List[Any]): FPile[E, U, C] = new FPile[E, U, C] {
    override val pathPile = self.pathPile
    //override val wrapPile = self.fShape.decodeData(dataList)
    override val fShape = self.fShape
  }

}

object FPile {
}

trait FsnShape[Packed_, DataType_, UnPacked_] {

  type Packed = Packed_
  type UnPacked = UnPacked_
  type DataType = DataType_

  def encodeColumn(pile: Packed_): List[FPath]
  def decodeColumn(columns: List[FPath]): Packed_

  def encodeData(pileData: UnPacked_): List[Any]
  def decodeData(data: List[Any]): UnPacked_

  def zero: UnPacked_
  slick.lifted.Shape

}