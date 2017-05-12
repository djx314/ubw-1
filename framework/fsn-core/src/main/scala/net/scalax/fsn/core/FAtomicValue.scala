package net.scalax.fsn.core

trait FAtomicValue {
  type DataType
  val atomic: Option[FAtomic[DataType]]
}

trait FAtomicValueImpl[D] extends FAtomicValue {
  self =>
  override val atomic: Option[FAtomic[D]]
  override type DataType = D
}

object FAtomicValueImpl {

  def apply[D](atomics: Option[FAtomic[D]]): FAtomicValueImpl[D] = {
    val atomics1 = atomics
    new FAtomicValueImpl[D] {
      override val atomic = atomics1
    }
  }

  def empty[D]: FAtomicValueImpl[D] = {
    new FAtomicValueImpl[D] {
      override val atomic = None
    }
  }

}