package net.scalax.ubw.core

import shapeless._

trait PileShape[Packed_, DataType_] {
  self =>

  type Packed = Packed_
  type DataType = DataType_

  def encodeColumn(pile: Packed_): List[AtomicPath]

  def encodeData(pileData: DataType_): List[AtomicValue]
  def decodeData(data: List[AtomicValue]): DataType_

  def zero: DataType_

  def dataLength: Int

}

object PileShape {

  val hnilPileShape: PileShape[HNil, HNil] = new PileShape[HNil, HNil] {
    self =>
    override def encodeColumn(pile: HNil): List[AtomicPath] = Nil
    override def encodeData(pileData: HNil): List[AtomicValue] = Nil
    override def decodeData(data: List[AtomicValue]): HNil = HNil
    override def zero = HNil
    override val dataLength = 0
  }

  def fpathPileShape[T]: PileShape[AtomicPathImpl[T], AtomicValueImpl[T]] =
    new PileShape[AtomicPathImpl[T], AtomicValueImpl[T]] {
      self =>
      override def encodeColumn(pile: AtomicPathImpl[T]): List[AtomicPath] = pile :: Nil
      override def encodeData(pileData: AtomicValueImpl[T]): List[AtomicValue] = pileData :: Nil
      override def decodeData(data: List[AtomicValue]): AtomicValueImpl[T] = data.head.asInstanceOf[AtomicValueImpl[T]]

      override def zero = AtomicValueImpl.empty

      override val dataLength = 1
    }

  def fpathHListPileShape[S, T <: HList, A, B <: HList](head: PileShape[S, A], tail: PileShape[T, B]): PileShape[S :: T, A :: B] = {
    new PileShape[S :: T, A :: B] {
      self =>
      override def encodeColumn(pile: S :: T): List[AtomicPath] = {
        val headPile :: tailPile = pile
        head.encodeColumn(headPile) ::: tail.encodeColumn(tailPile)
      }
      override def encodeData(pileData: A :: B): List[AtomicValue] = {
        val headData :: tailData = pileData
        head.encodeData(headData) ::: tail.encodeData(tailData)
      }
      override def decodeData(data: List[AtomicValue]): A :: B = {
        head.decodeData(data.take(head.dataLength)) :: tail.decodeData(data.drop(head.dataLength))
      }

      override def zero = head.zero :: tail.zero

      override val dataLength = head.dataLength + tail.dataLength
    }
  }
}