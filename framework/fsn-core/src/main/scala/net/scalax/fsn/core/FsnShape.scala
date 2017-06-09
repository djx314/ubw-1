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

  val hnilFsnShape: FsnShape[HNil, HNil] = new FsnShape[HNil, HNil] {
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

  def fpathHListFsnShape[S, T <: HList, A, B <: HList](head: FsnShape[S, A], tail: FsnShape[T, B]): FsnShape[S :: T, A :: B] = {
    new FsnShape[S :: T, A :: B] {
      self =>
      override def encodeColumn(pile: S :: T): List[FAtomicPath] = {
        val headPile :: tailPile = pile
        head.encodeColumn(headPile) ::: tail.encodeColumn(tailPile)
      }
      override def encodeData(pileData: A :: B): List[FAtomicValue] = {
        val headData :: tailData = pileData
        head.encodeData(headData) ::: tail.encodeData(tailData)
      }
      override def decodeData(data: List[FAtomicValue]): A :: B = {
        head.decodeData(data.take(head.dataLength)) :: tail.decodeData(data.drop(head.dataLength))
      }

      override def zero = head.zero :: tail.zero

      override val dataLength = head.dataLength + tail.dataLength
    }
  }
}