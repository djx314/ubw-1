package net.scalax.fsn.core

import scala.language.higherKinds
import shapeless._

trait FsnShape1111[Packed_, DataType_] {
  self =>

  type Packed = Packed_
  type DataType = DataType_

  def encodeColumn(pile: Packed_): List[FAtomicPath]

  def encodeData(pileData: DataType_): List[FAtomicValue]
  def decodeData(data: List[FAtomicValue]): DataType_

  def zero: DataType_

  val dataLength: Int

}

object FsnShape1111 {

  def hnilFsnShape1111[T[_]]: FsnShape1111[HNil, HNil] = new FsnShape1111[HNil, HNil] {
    self =>
    override def encodeColumn(pile: HNil): List[FAtomicPath] = Nil
    override def encodeData(pileData: HNil): List[FAtomicValue] = Nil
    override def decodeData(data: List[FAtomicValue]): HNil = HNil
    override def zero = HNil
    override val dataLength = 0
  }

  def fpathFsnShape1111[T, U[_]]: FsnShape1111[FAtomicPathImpl[T], FAtomicValueImpl[T]] =
    new FsnShape1111[FAtomicPathImpl[T], FAtomicValueImpl[T]] {
      self =>
      override def encodeColumn(pile: FAtomicPathImpl[T]): List[FAtomicPath] = pile :: Nil
      override def encodeData(pileData: FAtomicValueImpl[T]): List[FAtomicValue] = pileData :: Nil
      override def decodeData(data: List[FAtomicValue]): FAtomicValueImpl[T] = data.head.asInstanceOf[FAtomicValueImpl[T]]

      override def zero = new FAtomicValueImpl[T] {
        override val atomics = Nil
      }

      override val dataLength = 1
    }
}