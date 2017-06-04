package net.scalax.fsn.core

import shapeless._

trait FsnShape[Packed_, DataType_] {
  self =>

  type Packed = Packed_
  type DataType = DataType_

  def encodeColumn(pile: Packed_): List[FAtomicPath]

  def encodeData(pileData: DataType_): List[FAtomicValue]
  def decodeData(data: List[FAtomicValue]): DataType_

  def zero: DataType_

  def dataLength: Int

}

object FsnShape {

  def hnilFsnShape: FsnShape[HNil, HNil] = new FsnShape[HNil, HNil] {
    self =>
    override def encodeColumn(pile: HNil): List[FAtomicPath] = Nil
    override def encodeData(pileData: HNil): List[FAtomicValue] = Nil
    override def decodeData(data: List[FAtomicValue]): HNil = HNil
    override def zero = HNil
    override val dataLength = 0
  }

  def fpathFsnShape[T]: FsnShape[FAtomicPathImpl[T], FAtomicValueImpl[T]] =
    new FsnShape[FAtomicPathImpl[T], FAtomicValueImpl[T]] {
      self =>
      override def encodeColumn(pile: FAtomicPathImpl[T]): List[FAtomicPath] = pile :: Nil
      override def encodeData(pileData: FAtomicValueImpl[T]): List[FAtomicValue] = pileData :: Nil
      override def decodeData(data: List[FAtomicValue]): FAtomicValueImpl[T] = data.head.asInstanceOf[FAtomicValueImpl[T]]

      override def zero = FAtomicValueImpl.empty

      override val dataLength = 1
    }
}