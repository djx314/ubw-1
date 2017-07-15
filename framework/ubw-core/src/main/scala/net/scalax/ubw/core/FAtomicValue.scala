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

  override def toString = s"FAtomicValueImpl($atomics)"
}

object FAtomicValueImpl {

  def apply[D](atomics: Option[FAtomic[D]]): FAtomicValueImpl[D] = {
    val atomics1 = atomics
    new FAtomicValueImpl[D] {
      override val atomics = atomics1
    }
  }

  def empty[T]: FAtomicValueImpl[T] = new FAtomicValueImpl[T] {
    override val atomics = None
    override type DataType = T

    override def equals(obj: Any): Boolean = obj match {
      case elem: FAtomicValue if elem.atomics.isEmpty => true
      case _ => false
    }
  }

  //val Zero: FAtomicValue = empty[Nothing]

  object Zero {
    def unapply[T](fValue: FAtomicValueImpl[T]): Boolean = {
      if (empty[T].equals(fValue)) {
        //Option(())
        true
      } else {
        //None
        false
      }
    }
  }

}