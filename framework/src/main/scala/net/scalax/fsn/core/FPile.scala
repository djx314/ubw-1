package net.scalax.fsn.core

import scala.language.higherKinds
import scala.language.implicitConversions

trait FPileAbstract[C[_]] {
  self =>

  type PathType
  type DataType
  type WrapType[T] = C[T]

  val pathPile: PathType

  val fShape: FsnShape[PathType, DataType, WrapType]

  val prePile: FPile.SubPileType[DataType, WrapType]

}

case class FPileImpl[E, U, C[_]](
  override val pathPile: E,
  override val fShape: FsnShape[E, U, C],
  override val prePile: FPile.SubPileType[U, C]
) extends FPileAbstract[C] {
  override type PathType = E
  override type DataType = U
}

trait FEntity[C[_]] extends FPath {

  val data: C[DataType]

}

object FEntity {

  def changeData[C[_]](path: FPath)(newData: C[path.DataType]): FEntityImpl[path.DataType, C] = FEntityImpl(path.atomics, newData)

}

case class FEntityImpl[D, E[_]](override val atomics: List[FAtomic[D]], override val data: E[D]) extends FEntity[E] {
  override type DataType = D
}

object FPile {

  type SubPileType[DataType, WrapType[_]] = (List[Any] => DataType, List[FPileAbstract[WrapType]])

  def apply[E, U, C[_]](paths: E)(implicit shape: FsnShape[E, U, C]): FPileImpl[E, U, C] = {
    FPileImpl(paths, shape, { _: List[Any] => shape.zero } -> List.empty[FPileAbstract[C]])
  }

  def transform[C[_]](piles: List[FEntity[C]] => List[Any]): List[FEntity[C]] => List[FEntity[C]] = {
    (oldPiles: List[FEntity[C]]) => {
      oldPiles.zip(piles(oldPiles)).map { case (entity, data) =>
        FEntity.changeData(entity)(data.asInstanceOf[C[entity.DataType]])
      }
    }
  }

  def transformOpt(piles: List[FEntity[Option]] => List[Any]): List[FEntity[Option]] => List[FEntity[Option]] = {
    (oldPiles: List[FEntity[Option]]) => {
      oldPiles.zip(piles(oldPiles)).map { case (entity, data) =>
        FEntity.changeData(entity)(data.asInstanceOf[Option[entity.DataType]])
      }
    }
  }

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

  val dataLength: Int

}