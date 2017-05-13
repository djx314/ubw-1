package net.scalax.fsn.core

trait FAtomicValue extends FAtomicWrap {
  override type DataType
  override type CollType[T] = Option[T]
  override val atomics: Option[FAtomic[DataType]]
}

trait FAtomicValueImpl[D] extends FAtomicValue {
  self =>
  override val atomics: Option[FAtomic[D]]
  override type DataType = D
}

object FAtomicValueImpl {

  def apply[D](atomics: Option[FAtomic[D]]): FAtomicValueImpl[D] = {
    val atomics1 = atomics
    new FAtomicValueImpl[D] {
      override val atomics = atomics1
    }
  }

  def empty[D]: FAtomicValueImpl[D] = {
    new FAtomicValueImpl[D] {
      override val atomics = None
    }
  }

}