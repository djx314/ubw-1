package net.scalax.fsn.core

import scala.language.higherKinds
import shapeless._

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

object FsnShape {

  implicit def hlistFsnShape[T[_]]: FsnShape[HNil, HNil, T] = new FsnShape[HNil, HNil, T] {
    override def encodeColumn(pile: HNil): List[FPath] = Nil
    override def decodeColumn(columns: List[FPath]): HNil = HNil

    override def encodeData(pileData: HNil): List[T[Any]] = Nil
    override def decodeData(data: List[T[Any]]): HNil = HNil

    override def zero = HNil

    override val dataLength = 0
  }

  implicit def hlistFsnShapeaa[T, U[_]](implicit zeroPile: FZeroPile[U[T]]): FsnShape[FPathImpl[T], U[T], U] = new FsnShape[FPathImpl[T], U[T], U] {
    override def encodeColumn(pile: FPathImpl[T]): List[FPath] = pile :: Nil
    override def decodeColumn(columns: List[FPath]): FPathImpl[T] = columns.head.asInstanceOf[FPathImpl[T]]

    override def encodeData(pileData: U[T]): List[U[Any]] = pileData.asInstanceOf[U[Any]] :: Nil
    override def decodeData(data: List[U[Any]]): U[T] = data.head.asInstanceOf[U[T]]

    override def zero = zeroPile.zero

    override val dataLength = 1
  }

  /*implicit def fAtomicFsnShape[T, U[_]](implicit zeroPile: FZeroPile[U[T]]): FsnShape[List[FAtomic[T]], U[T], U] = new FsnShape[List[FAtomic[T]], U[T], U] {
    override def encodeColumn(atomics: List[FAtomic[T]]): List[FPath] = FPathImpl(atomics) :: Nil
    override def decodeColumn(columns: List[FPath]): List[FAtomic[T]] = columns.head.asInstanceOf[FPathImpl[T]].atomics

    override def encodeData(pileData: U[T]): List[U[Any]] = pileData.asInstanceOf[U[Any]] :: Nil
    override def decodeData(data: List[U[Any]]): U[T] = data.head.asInstanceOf[U[T]]

    override def zero = zeroPile.zero

    override val dataLength = 1
  }*/

  implicit def jfkoajiroejhteiroth[S <: HList, T <: HList, U, W, V <: HList, A[_]](
    implicit
    cv: S <:< (U :: T),
    reverseCv: (U :: T) <:< S,
    subShape: FsnShape[U, W, A],
    tailShape: FsnShape[T, V, A]
  ): FsnShape[S, W :: V, A] = new FsnShape[S, W :: V, A] {
    override def encodeColumn(pile: S): List[FPath] = {
      val headPile :: hlistPile = cv(pile)
      subShape.encodeColumn(headPile) ::: tailShape.encodeColumn(hlistPile)
    }
    override def decodeColumn(columns: List[FPath]): S = {
      reverseCv(subShape.decodeColumn(columns.take(subShape.dataLength)) :: tailShape.decodeColumn(columns.drop(subShape.dataLength)))
    }

    override def encodeData(pileData: W :: V): List[A[Any]] = {
      val headData :: tailData = pileData
      subShape.encodeData(headData) ::: tailShape.encodeData(tailData)
    }
    override def decodeData(data: List[A[Any]]): W :: V = {
      subShape.decodeData(data.take(subShape.dataLength)) :: tailShape.decodeData(data.drop(subShape.dataLength))
    }

    override def zero = subShape.zero :: tailShape.zero

    override val dataLength = subShape.dataLength + tailShape.dataLength
  }

}