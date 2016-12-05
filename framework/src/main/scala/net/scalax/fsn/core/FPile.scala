package net.scalax.fsn.core

import scala.language.higherKinds
import scala.language.implicitConversions

trait FPileAbstract[C[_]] {
  self =>

  type PathType
  type DataType
  type WrapType[T] = C[T]

  val pathPile: PathType
  val data: Option[DataType]

  val fShape: FsnShape[PathType, DataType, WrapType]

  val prePile: FPile.SubPileType[DataType, WrapType]

}

case class FPileImpl[E, U, C[_]](
  override val pathPile: E,
  override val data: Option[U],
  override val fShape: FsnShape[E, U, C],
  override val prePile: FPile.SubPileType[U, C]
) extends FPileAbstract[C] {
  override type PathType = E
  override type DataType = U
}

object FPile {

  type SubPileType[DataType, WrapType[_]] = (List[Any] => DataType, List[FPileAbstract[WrapType]])

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