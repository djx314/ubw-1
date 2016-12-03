package net.scalax.fsn.core

import scala.language.higherKinds
import scala.language.implicitConversions

trait FPile[E, U, C[_]] {
  self =>

  type PathType = E
  type DataType = U
  type WrapType[T] = C[T]

  val pathPile: PathType

  val fShape: FsnShape[PathType, DataType, WrapType]

  val prePile: List[FPileWrap[DataType, WrapType]]

}

object FPile {
}

trait FPileWrap[P, C[_]] {

  type PathType
  type DataType
  type ParentDataType = P
  type WrapType[T] = C[T]

  val convert: DataType => ParentDataType
  val pile: FPile[PathType, DataType, WrapType]

}

trait FsnShape[Packed_, DataType_, UnPacked_[_]] {

  type Packed = Packed_
  type UnPacked[T] = UnPacked_[T]
  type DataType = DataType_

  def encodeColumn(pile: Packed_): List[FPath]
  def decodeColumn(columns: List[FPath]): Packed_

  def encodeData(pileData: DataType_): List[UnPacked_[Any]]
  def decodeData(data: List[UnPacked_[Any]]): DataType_

  def zero: DataType_

}